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
  [track-num: (exact-nonnegative-integer? . -> . metadata-entry?)]))

; use '(require (submod "./metadata.rkt" metadata-entry-struct))' to get the accessors for
; the 'metadata-entry' struct (this way they are not publicly accessible)
(module* metadata-entry-struct #f
  (provide
   metadata-entry-key
   metadata-entry-value))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Keys
;; --------------------

(struct metadata-key [pretty]
  #:methods gen:custom-write
  [(define (write-proc m-k port mode)
     (display (metadata-key-pretty m-k) port))])

(define-syntax-rule (define-metadata-keys m-k->symbol-id
                      [m-k-id ([m-k-fmt m-k-sym-expr] ...)]
                      ...)
  (begin
    ;; each m-k-id is defined as a value that displays as itself when printed
    (define-values [m-k-id ...]
      (values (metadata-key 'm-k-id) ...))

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
  [+title     ([mp3 'title] [ogg 'TITLE])]
  [+album     ([mp3 'album] [ogg 'ALBUM])]
  [+artist    ([mp3 'album_artist] [ogg 'ARTIST])]
  [+track-num ([mp3 'track] [ogg 'TRACKNUMBER])])

(module+ test
  (check-equal? (metadata-key->symbol +title 'mp3) 'title)
  (check-equal? (metadata-key->symbol +album 'ogg) 'ALBUM)
  (check-equal? (metadata-key->symbol +track-num 'mp3) 'track)
  (check-equal? (metadata-key->symbol +track-num 'ogg) 'TRACKNUMBER)
  (check-equal? (metadata-key->symbol +title 'flac) #f))

;; ---------------------------------------------------------------------------------------
;; Entries
;; --------------------

(struct metadata-entry [key value]
  #:transparent
  #:extra-constructor-name meta:
  #:methods gen:custom-write
  [(define (write-proc m-e port mode)
     (write `(meta: ,(metadata-entry-key m-e)
                    ,(metadata-entry-value m-e))
            port))])

(define (title: s)     (meta: +title s))
(define (album: s)     (meta: +album s))
(define (artist: s)    (meta: +artist s))
(define (track-num: n) (meta: +track-num (format "~a" n)))
