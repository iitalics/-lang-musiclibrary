#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; metadata
 metadata-key? metadata-entry?
 +title +album +track-num
 (contract-out
  [meta: (metadata-key? string? . -> . metadata-entry?)]
  [metadata-key->symbol (metadata-key? symbol? . -> . (or/c symbol? #f))]
  [title: (string? . -> . metadata-entry?)]
  [album: (string? . -> . metadata-entry?)]
  [track-num: (exact-nonnegative-integer? . -> . metadata-entry?)])
 ; ---
 ; album
 album?
 (contract-out
  [album (() #:rest (listof track?) . ->* . album?)])
 ; ---
 ; track
 track? source?
 in-track-metadata
 (contract-out
  [rename track* track
          ((source? #:output path-string?)
           #:rest (listof metadata-entry?)
           . ->* . track?)]
  [track-audio-src (track? . -> . source?)]
  [track-output-path (track? . -> . path?)]
  [track-metadata (track? metadata-key? . -> . (or/c string? #f))]
  [track-title (track? . -> . string?)]
  [track-number (track? . -> . (or/c exact-integer? #f))])
 ; ---
 ; current-tracks-to-generate
 with-generate-tracks
 (contract-out
  [rename current-tracks-to-generate*
          current-tracks-to-generate
          (-> (set/c track?))]
  [queue-track! (track? . -> . any)]
  [queue-album! (album? . -> . any)]))

(require
 racket/set)

(module+ test
  (require rackunit racket/port))

;; ---------------------------------------------------------------------------------------
;; Metadata
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
  [+track-num ([mp3 'track] [ogg 'TRACKNUMBER])])

(struct metadata-entry [key value]
  #:transparent
  #:extra-constructor-name meta:)

(define (title: s) (meta: +title s))
(define (album: s) (meta: +album s))
(define (track-num: n) (meta: +track-num (format "~a" n)))

(module+ test
  (check-equal? (metadata-key->symbol +title 'mp3) 'title)
  (check-equal? (metadata-key->symbol +album 'ogg) 'ALBUM)
  (check-equal? (metadata-key->symbol +track-num 'mp3) 'track)
  (check-equal? (metadata-key->symbol +track-num 'ogg) 'TRACKNUMBER)
  (check-equal? (metadata-key->symbol +title 'flac) #f))

;; ---------------------------------------------------------------------------------------
;; Albums and tracks
;; --------------------

;; album ::= [listof track]
(define (album? x)
  (and (list? x)
       (andmap track? x)))

;; track ... -> album
(define (album . ts)
  ts)

;; source ::= path
(define (source? x)
  (path? x))

;; audio-src : source
;; output-path : path
;; meta : (hasheq metadata-key => string)
(struct track [audio-src
               output-path
               meta]
  #:extra-constructor-name make-track
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (fprintf port "#<track:~s>" (track-title t)))])

;; track metadata-key -> (or string #f)
(define (track-metadata trk k)
  (hash-ref (track-meta trk) k #f))

;; track -> [sequenceof (values metadata-key string)]
(define-syntax-rule (in-track-metadata trk)
  (in-hash (track-meta trk)))

;; (track* audio-src #:output out-path m-e ...)
;; audio-src : source
;; out-path : path-string
;; m-e : metadata-entry
(define (track** audio-src
                 #:output out-path
                 . m-es)
  (make-track audio-src
              (build-path out-path)
              (for/hash ([m-e (in-list m-es)])
                (values (metadata-entry-key m-e)
                        (metadata-entry-value m-e)))))

(define track* (procedure-rename track** 'track))

;; track -> string
(define (track-title t)
  (or (track-metadata t +title)
      "(no title)"))

;; track-number -> string
(define (track-number t)
  (cond [(track-metadata t +track-num) => string->number]
        [else #f]))

;; ---------------------------------------------------------------------------------------
;; Tracks queue
;; --------------------

;; (current-tracks-to-generate) : (or #f [seteqof track])
(define current-tracks-to-generate
  (make-parameter #f))

;; (current-tracks-to-generate*) : [seteqof track]
(define (current-tracks-to-generate*)
  (define ts (current-tracks-to-generate))
  (unless ts
    (error 'current-tracks-to-generate
           "tracks queue not initialized; cannot add track"))
  ts)

;; [-> any] -> [seteqof tracks]
(define (call/generate-tracks f)
  (parameterize ([current-tracks-to-generate (seteq)])
    (f)
    (current-tracks-to-generate)))

(define-syntax-rule (with-generate-tracks body ...)
  (call/generate-tracks (λ () body ...)))

;; (queue-track! t) : void
;; t : track
(define (queue-track! t)
  (queue-album! (album t)))

;; (queue-album! a) : void
;; a : album
(define (queue-album! alb)
  (current-tracks-to-generate
   (for/fold ([ts (current-tracks-to-generate*)])
             ([t  (in-list alb)])
     (set-add ts t))))

;; =======================================================================================

(module+ test
  (define P string->path)

  ;; ---------------
  ;; track

  (define t1 (track* (P "a") #:output (P "A") (title: "aaa")))
  (define t2 (track* (P "b") #:output (P "B") (title: "bbb")))
  (define t3 (track* (P "c") #:output (P "C") (title: "ccc") (track-num: "4")))

  (check-equal?
   (with-output-to-string (λ () (display t2)))
   "#<track:\"bbb\">")

  (check-equal? (track-number t2) #f)
  (check-equal? (track-number t3) 4)

  ;; ---------------
  ;; current-tracks-to-generate

  (check-exn #px"not init"
             (λ () (current-tracks-to-generate*)))

  (check-equal?
   (with-generate-tracks
     (queue-track! t1))
   (seteq t1))

  (check-equal?
   (with-generate-tracks
     (queue-track! t1)
     (with-generate-tracks
       (queue-track! t2)))
   (seteq t1))

  (check-equal?
   (with-generate-tracks
     (queue-track! t1)
     (queue-album! (album t2 t3))
     (queue-track! t3))
   (seteq t1 t2 t3)))
