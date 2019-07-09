#lang racket/base
(require racket/contract)

(provide
 ;; ---
 ;; keys
 prop:metadata-key
 metadata-key?
 ; +title +album +track-num +artist
 ;; ---
 ;; entries
 metadata-entry?
 #;
 (contract-out
  [meta:      (metadata-key? string?      . -> . metadata-entry?)]
  [title:     (string?                    . -> . metadata-entry?)]
  [album:     (string?                    . -> . metadata-entry?)]
  [artist:    (string?                    . -> . metadata-entry?)]
  [track-num: (exact-nonnegative-integer? . -> . metadata-entry?)]
  [apply-metadata-entry ((metadata-entry?
                          ffmpeg-args?
                          #:format symbol?)
                         . ->* . ffmpeg-args?)]))

; use '(require (submod "./metadata.rkt" metadata-entry-struct))' to get the accessors for
; the 'metadata-entry' struct (this way they are not publicly accessible)
(module* metadata-entry-struct #f
  (provide
   metadata-entry-key
   metadata-entry-value))

(require
 "./ffmpeg.rkt")

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Types
;; ----------

;; prop:metadata-key : struct-type-property
;; where
;; metadata-key-ref : self -> (or [self any -> metadata-entry])
;;                                [self ffmpeg-args symbol any -> ffmpeg-args])
(define-values [prop:metadata-key metadata-key? metadata-key-ref]
  (make-struct-type-property 'metadata-key
                             #f
                             '()))

;; key : metadata-key
;; val : any
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
     (define m-e (proc m-k val))
     (apply-metadata-key (metadata-entry-key m-e)
                         f-a
                         #:value (metadata-entry-value m-e)
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

(module+ test

  (struct simple-key [stuff]
    #:property prop:metadata-key
    (λ (self f-a fmt val)
      `(SIMPLE ,f-a ,fmt ,val ,(simple-key-stuff self))))

  (struct doubling-key [stuff]
    #:property prop:metadata-key
    (λ (self val)
      (meta: (simple-key (* 2 (doubling-key-stuff self)))
             (* 2 val))))

  (let ([args0 (make-ffmpeg-args (build-path "O")
                                 #:asrc-path (build-path "A")
                                 #:asrc-flags '())])

    (check-equal? (apply-metadata-key (simple-key 6)
                                      args0
                                      #:value 5
                                      #:format 'mp3)
                  `(SIMPLE ,args0 mp3 5 6))

    (check-equal? (apply-metadata-key (doubling-key 6)
                                      args0
                                      #:value 5
                                      #:format 'ogg)
                  `(SIMPLE ,args0 ogg 10 12))))

;; ---------------------------------------------------------------------------------------
;; Key defining helpers
;; ----------

#;
(define-metadata-keys metadata-key->symbol
  [+title     title:     ([mp3 'title] [ogg 'TITLE])]
  [+album     album:     ([mp3 'album] [ogg 'ALBUM])]
  [+artist    artist:    ([mp3 'album_artist] [ogg 'ARTIST])]
  [+track-num track-num:* ([mp3 'track] [ogg 'TRACKNUMBER])])

#;
(define (track-num: n)
  (track-num:* (format "~a" n)))

#;
(module+ test
  (check-equal? (title: "A") (meta: +title "A"))
  (check-equal? (track-num: 5) (meta: +track-num "5")))

#;
(ffmpeg-args-set-metadata f-a
                          (metadata-key->symbol (metadata-entry-key m-e) fmt)
                          (metadata-entry-value m-e))
