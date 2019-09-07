#lang racket/base
(require racket/contract)

(provide
 ;; ---
 ;; keys
 prop:metadata-key
 metadata-key?
 +title +album +track-num +artist +cover-art
 ;; ---
 ;; entries
 metadata-entry?
 (contract-out
  [meta:      (metadata-key? any/c        . -> . metadata-entry?)]
  [title:     (string?                    . -> . metadata-entry?)]
  [album:     (string?                    . -> . metadata-entry?)]
  [artist:    (string?                    . -> . metadata-entry?)]
  [track-num: (exact-nonnegative-integer? . -> . metadata-entry?)]
  [cover-art: (source?                    . -> . metadata-entry?)]
  [apply-metadata-entry ((metadata-entry?
                          ffmpeg-args?
                          #:format symbol?
                          #:cache source-cache?)
                         . ->* .
                         ffmpeg-args?)])
 ;; ---
 ;; macros
 define-metadata-key
 define-simple-metadata-key)

; use '(require (submod "./metadata.rkt" metadata-entry-struct))' to get the accessors for
; the 'metadata-entry' struct (this way they are not publicly accessible)
(module* metadata-entry-struct #f
  (provide
   metadata-entry-key
   metadata-entry-value))

(require
 "./ffmpeg.rkt"
 "./source.rkt"
 "./source/cache.rkt"
 syntax/parse/define
 (for-syntax racket/base
             racket/syntax))

(module+ test
  (require rackunit racket/match))

;; ---------------------------------------------------------------------------------------
;; Structs / properties
;; ----------

;; Implement this property to create a new kind of metadata key. It is advised to use
;; 'define-metadata-key' or 'define-simple-metadata-key' instead of using this property
;; directly.
(define-values [prop:metadata-key metadata-key? metadata-key-ref]
  (make-struct-type-property 'metadata-key
                             #f
                             '()))

;; key : metadata-key
;; value : any
(struct metadata-entry [key value]
  #:transparent
  #:extra-constructor-name meta:
  #:methods gen:custom-write
  [(define (write-proc m-e port mode)
     (write `(meta: ,(metadata-entry-key m-e)
                    ,(metadata-entry-value m-e))
            port))])

;; (apply-metadata-entry m-e f-a #:format fmt #:cache cache) : ffmpeg-args
;; m-e : metadata-entry
;; f-a : ffmpeg-args
;; fmt : symbol
;; cache : source-cache
;; --
;; raises exn:fail:source-cache-miss if any required sources were not found
;; otherwise returns the arguments updated with additional flags needed to apply
;; the metadata.
(define (apply-metadata-entry m-e f-a #:format fmt #:cache sc)
  (apply-metadata-key (metadata-entry-key m-e)
                      f-a
                      #:value (metadata-entry-value m-e)
                      #:format fmt
                      #:cache sc))

;; (apply-metadata-key m-k f-a #:value val #:format fmt #:cache sc) : ffmpeg-args
;; m-k : metadata-key
;; f-a : ffmpeg-args
;; val : any
;; fmt : symbol
;; sc : source-cache
(define (apply-metadata-key m-k f-a #:value val #:format fmt #:cache sc)
  (define proc (metadata-key-ref m-k))
  (cond
    [(procedure-arity-includes? proc 2)
     (apply-metadata-entry (proc m-k val) f-a #:format fmt #:cache sc)]
    [(procedure-arity-includes? proc 5)
     (proc m-k f-a fmt sc val)]
    [else
     (error 'apply-metadata-key
            (format "~a's prop:metadata-key value should be function of 2 or 5 arguments"
                    m-k))]))

;; ---------------------------------------------------------------------------------------
;; Key defining helpers
;; ----------

;; value : any
(struct constant-write [value]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write (constant-write-value self) port))])

;; ----

(begin-for-syntax
  (define-syntax-class base-id
    [pattern x:id
             #:with k (format-id #'x "+~a" #'x)
             #:with c (format-id #'x "~a:" #'x)]))

(define-simple-macro (define-key+constructor name:base-id
                       pre-body ...
                       key-expr)
  (define-values [name.k name.c]
    (let ()
      pre-body ...
      (define key key-expr)
      (values key (λ (v) (meta: key v))))))

;; --------------------------------------------------------------------------------
;; define-metadata-key
;; --------------------

;; (define-metadata-key (<base-name-id> <args>)
;;   <body> ...)
;;
;; <args> ::= <val-id>
;;          | <ffmpeg-args-id> <fmt-id> <val-id>
;; --
;; TODO: this interface kind of sucks, and it's not used very often :S maybe do something
;; different
(define-simple-macro (define-metadata-key (name:base-id arg:id ...)
                       body ...)
  #:fail-unless (member (length (attribute arg)) '(1 4))
  (format "expects either 1 or 4 arguments, got ~a" (length (attribute arg)))

  (define-key+constructor name
    (struct key constant-write []
      #:property prop:metadata-key
      (λ (_ arg ...) body ...))

    (key 'name.k)))

;; ==========

(module+ test
  (define-metadata-key (as-list f-a fmt spc val)
    `(,f-a ,fmt ,(hash-keys spc) ,val))

  (define-metadata-key (as-list-doubled val)
    (as-list: (* 2 val)))

  (define args0 (make-ffmpeg-args (build-path "O")))
  (define cache0 (hash (fs "a.png") (build-path "cache/a.png")))

  (check-equal? (apply-metadata-entry (as-list: 5)
                                      args0
                                      #:cache cache0
                                      #:format 'mp3)
                `(,args0 mp3 [,(fs "a.png")] 5))

  (check-equal? (apply-metadata-entry (as-list-doubled: 5)
                                      args0
                                      #:cache cache0
                                      #:format 'ogg)
                `(,args0 ogg [,(fs "a.png")] 10)))

;; --------------------------------------------------------------------------------
;; define-simple-metadata-key
;; --------------------

;; (define-simple-metadata-key <base-name-id>
;;   [<fmt> <symbol-expr>]
;;   ...
;;   <option>
;;   ...)
;;
;; <fmt>    ::= <id>
;; <option> ::= #:->string <to-string-expr>
;; --
(define-simple-macro (define-simple-metadata-key name:base-id
                       [fmt:id fmt->sym-expr:expr]
                       ...
                       {~optional {~seq #:->string ->s-expr:expr}
                                  #:defaults ([->s-expr #'values])})
  (define-key+constructor name
    (define fmt=>sym (make-hasheq (list (cons 'fmt fmt->sym-expr) ...)))
    (define (fmt->sym f) (hash-ref fmt=>sym f #f))

    (simple-key 'name.k fmt->sym ->s-expr)))

;; format->symbol : symbol -> (or symbol #f)
;; value->string  : any -> string
(struct simple-key constant-write [format->symbol value->string]
  #:property prop:metadata-key
  (λ (self f-a fmt _cache val)
    (ffmpeg-args-set-metadata f-a
                              ((simple-key-format->symbol self) fmt)
                              ((simple-key-value->string self) val))))

;; ---
;; Standard metadata

(define-simple-metadata-key title     [mp3 'title] [ogg 'TITLE])
(define-simple-metadata-key album     [mp3 'album] [ogg 'ALBUM])
(define-simple-metadata-key artist    [mp3 'album_artist] [ogg 'ARTIST])
(define-simple-metadata-key track-num [mp3 'track] [ogg 'TRACKNUMBER]
  #:->string number->string)

;; ---
;; More metadata

;; (cover-art: img-src) : metadata-entry
;; img-src : source
(define-metadata-key (cover-art ffm-args _fmt cache img-src)
  (ffmpeg-args-add-input ffm-args
                         (source-cache-ref cache img-src)
                         '()))

;; ==========

(module+ test
  (check-equal? (title: "A") (meta: +title "A"))
  (check-equal? (track-num: 3) (meta: +track-num 3))

  (check-equal? (format "~a" +title) "+title")
  (check-equal? (format "~a" (title: "x")) "(meta: +title \"x\")")

  (check-equal? (apply-metadata-entry (artist: "me")
                                      args0
                                      #:cache cache0
                                      #:format 'mp3)
                (ffmpeg-args-set-metadata args0 'album_artist "me"))

  (check-equal? (apply-metadata-entry (title: "foo")
                                      args0
                                      #:cache cache0
                                      #:format 'ogg)
                (ffmpeg-args-set-metadata args0 'TITLE "foo"))

  (check-equal? (apply-metadata-entry (track-num: 3)
                                      args0
                                      #:cache cache0
                                      #:format 'ogg)
                (ffmpeg-args-set-metadata args0 'TRACKNUMBER "3"))

  (check-equal? (apply-metadata-entry (cover-art: (fs "a.png"))
                                      args0
                                      #:cache cache0
                                      #:format 'ogg)
                (ffmpeg-args-add-input args0 (build-path "cache/a.png") '()))

  (check-exn (match-lambda
               [(exn:fail:source-cache-miss _ _ src)
                (equal? src (fs "b.png"))]
               [_ #f])
             (λ ()
               (apply-metadata-entry (cover-art: (fs "b.png"))
                                     args0
                                     #:cache cache0
                                     #:format 'ogg))))
