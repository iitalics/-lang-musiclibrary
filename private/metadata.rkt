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
                          #:format symbol?)
                         . ->* . ffmpeg-args?)])
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
 syntax/parse/define
 (for-syntax racket/base
             racket/syntax))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Structs / properties
;; ----------

;; prop:metadata-key : struct-type-property
;; where
;; metadata-key-ref : self -> (or [self any -> metadata-entry])
;;                                [self ffmpeg-args symbol any -> ffmpeg-args])
;; --
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

;; (apply-metadata-key m-k f-a #:value val #:format fmt) : ffmpeg-args
;; m-k : metadata-key
;; f-a : ffmpeg-args
;; val : any
;; fmt : symbol
(define (apply-metadata-key m-k f-a #:value val #:format fmt)
  (define proc (metadata-key-ref m-k))
  (cond
    [(procedure-arity-includes? proc 2)
     (apply-metadata-entry (proc m-k val)
                           f-a
                           #:format fmt)]
    [(procedure-arity-includes? proc 4)
     (proc m-k f-a fmt val)]
    [else
     (error 'apply-metadata-key
            "prop:metadata-key value should be function of 1 or 4 arguments")]))

;; (apply-metadata-entry m-e f-a #:format fmt) : ffmpeg-args
;; m-e : metadata-entry
;; f-a : ffmpeg-args
;; fmt : symbol
(define (apply-metadata-entry m-e f-a #:format fmt)
  (apply-metadata-key (metadata-entry-key m-e)
                      f-a
                      #:value (metadata-entry-value m-e)
                      #:format fmt))

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
(define-simple-macro (define-metadata-key (name:base-id arg:id ...)
                       body ...)
  #:fail-unless (member (length (attribute arg)) '(1 3))
  (format "expects either 1 or 3 arguments, got ~a" (length (attribute arg)))

  (define-key+constructor name
    (struct key constant-write []
      #:property prop:metadata-key
      (λ (_ arg ...) body ...))

    (key 'name.k)))

;; ==========

(module+ test

  (define args0
    (make-ffmpeg-args (build-path "O")))

  (define-metadata-key (as-list f-a fmt val)
    `(,f-a ,fmt ,val))

  (define-metadata-key (as-list-doubled val)
    (as-list: (* 2 val)))

  (check-equal? (apply-metadata-entry (as-list: 5) args0 #:format 'mp3)
                `(,args0 mp3 5))

  (check-equal? (apply-metadata-entry (as-list-doubled: 5) args0 #:format 'ogg)
                `(,args0 ogg 10)))

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
  (λ (self f-a fmt val)
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
(define-metadata-key (cover-art ffm-args fmt img-src)
  (define img-path (source-fetch img-src))
  (ffmpeg-args-add-input ffm-args img-path '()))

;; ==========

(module+ test
  (check-equal? (title: "A") (meta: +title "A"))
  (check-equal? (track-num: 3) (meta: +track-num 3))

  (check-equal? (format "~a" +title) "+title")
  (check-equal? (format "~a" (title: "x")) "(meta: +title \"x\")")

  (check-equal? (apply-metadata-entry (artist: "me") args0 #:format 'mp3)
                (ffmpeg-args-set-metadata args0 'album_artist "me"))

  (check-equal? (apply-metadata-entry (title: "foo") args0 #:format 'ogg)
                (ffmpeg-args-set-metadata args0 'TITLE "foo"))

  (check-equal? (apply-metadata-entry (track-num: 3) args0 #:format 'ogg)
                (ffmpeg-args-set-metadata args0 'TRACKNUMBER "3"))

  (define eg-path (build-path "../example/lain.png"))
  (check-equal? (apply-metadata-entry (cover-art: (fs eg-path)) args0 #:format 'ogg)
                (ffmpeg-args-add-input args0 eg-path '()))

  (check-exn exn:fail:fetch?
             (λ ()
               (apply-metadata-entry (cover-art: (fs "../example/doesnt-exist.png"))
                                     args0
                                     #:format 'ogg))))
