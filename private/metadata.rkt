#lang racket/base
(require racket/contract)

(provide
 ;; ---
 ;; keys
 metadata-key?
 +title +album +track-num +artist
 (contract-out
  [metadata-key->symbol (metadata-key? symbol? . -> . (or/c symbol? #f))])
 ;; ---
 ;; entries
 metadata-entry?
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
;; Keys / Entries
;; ----------

(struct metadata-key [pretty]
  #:methods gen:custom-write
  [(define (write-proc m-k port mode)
     (display (metadata-key-pretty m-k) port))])

(struct metadata-entry [key value]
  #:transparent
  #:extra-constructor-name meta:
  #:methods gen:custom-write
  [(define (write-proc m-e port mode)
     (write `(meta: ,(metadata-entry-key m-e)
                    ,(metadata-entry-value m-e))
            port))])

(define-syntax-rule (define-metadata-keys m-k->symbol-id
                      [m-k-id m:-id ([m-k-fmt m-k-sym-expr] ...)]
                      ...)
  (begin
    ;; each m-k-id is defined as a value that displays as itself when printed
    (define-values [m-k-id ...]
      (values (metadata-key 'm-k-id) ...))

    (define (m:-id v)
      (meta: m-k-id v))
    ...

    ;; metadata-key? symbol -> (or symbol #f)
    (define (m-k->symbol-id m-k fmt)
      (cond
        [(eq? m-k m-k-id)
         (case fmt
           [(m-k-fmt) m-k-sym-expr]
           ...
           [else #f])]
        ...))))

(define-metadata-keys metadata-key->symbol
  [+title     title:     ([mp3 'title] [ogg 'TITLE])]
  [+album     album:     ([mp3 'album] [ogg 'ALBUM])]
  [+artist    artist:    ([mp3 'album_artist] [ogg 'ARTIST])]
  [+track-num track-num:* ([mp3 'track] [ogg 'TRACKNUMBER])])

(define (track-num: n)
  (track-num:* (format "~a" n)))

(module+ test
  (check-equal? (metadata-key->symbol +title 'mp3) 'title)
  (check-equal? (metadata-key->symbol +album 'ogg) 'ALBUM)
  (check-equal? (metadata-key->symbol +track-num 'mp3) 'track)
  (check-equal? (metadata-key->symbol +track-num 'ogg) 'TRACKNUMBER)
  (check-equal? (metadata-key->symbol +title 'flac) #f)

  (check-equal? (title: "A") (meta: +title "A"))
  (check-equal? (track-num: 5) (meta: +track-num "5")))

;; ---------------------------------------------------------------------------------------
;; Applying
;; ----------

;; metadata-entry ffmpeg-args symbol -> metadata-entry
(define (apply-metadata-entry m-e f-a #:format fmt)
  (ffmpeg-args-set-metadata f-a
                            (metadata-key->symbol (metadata-entry-key m-e) fmt)
                            (metadata-entry-value m-e)))
