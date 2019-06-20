#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; metadata
 metadata-key? metadata-entry?
 +title +album +track-num +artist
 (contract-out
  [metadata-key->symbol (metadata-key? symbol? . -> . (or/c symbol? #f))]
  [meta:      (metadata-key? string?      . -> . metadata-entry?)]
  [title:     (string?                    . -> . metadata-entry?)]
  [album:     (string?                    . -> . metadata-entry?)]
  [artist:    (string?                    . -> . metadata-entry?)]
  [track-num: (exact-nonnegative-integer? . -> . metadata-entry?)])
 ; ---
 ; album
 album?
 in-album-tracks
 (contract-out
  [rename album* album
          (((or/c string? #f)) ; title
           #:rest
           (listof (or/c metadata-entry?
                         track?
                         (listof track?)))
           . ->* . album?)]
  [album/single (track? . -> . album?)]
  [album-title (album? . -> . string?)]
  [album-tracks (album? . -> . (listof track?))])
 ; ---
 ; track
 track? source?
 in-track-metadata
 (contract-out
  [rename track* track
          ((#:audio source?
            #:output path-string?)
           #:rest (listof metadata-entry?)
           . ->* . track?)]
  [track-audio-src (track? . -> . source?)]
  [track-output-path (track? . -> . path?)]
  [track-metadata (track? metadata-key? . -> . (or/c string? #f))]
  [track-title (track? . -> . string?)]
  [track-number (track? . -> . (or/c exact-integer? #f))]))

(require
 (only-in racket/hash hash-union))

(module+ test
  (require rackunit racket/port))

;; ---------------------------------------------------------------------------------------
;; Utils
;; --------------------

(define (rev-append xs/rev ys)
  (foldl cons ys xs/rev))

(define (left-biased x y) x)

(module+ test
  (check-equal? (rev-append '(3 2 1) '(4 5 6))
                '(1 2 3 4 5 6)))

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
  [+artist    ([mp3 'album_artist] [ogg 'ARTIST])]
  [+track-num ([mp3 'track] [ogg 'TRACKNUMBER])])

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

(module+ test
  (check-equal? (metadata-key->symbol +title 'mp3) 'title)
  (check-equal? (metadata-key->symbol +album 'ogg) 'ALBUM)
  (check-equal? (metadata-key->symbol +track-num 'mp3) 'track)
  (check-equal? (metadata-key->symbol +track-num 'ogg) 'TRACKNUMBER)
  (check-equal? (metadata-key->symbol +title 'flac) #f))

;; ---------------------------------------------------------------------------------------
;; Tracks
;; --------------------

;; source ::= path
(define (source? x)
  (path? x))

;; audio-src : source
;; output-path : path
;; meta : (hasheq metadata-key => string)
(struct track [audio-src
               output-path
               meta]
  #:transparent
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

;; (track* #:audio audio-src #:output out-path m-e ...)
;; audio-src : source
;; out-path : path-string
;; m-e : metadata-entry
(define (track** #:audio audio-src
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

;; [hasheq metadata-key => string] track -> track
(define (add-meta-to-track meta trk)
  (struct-copy track trk
               [meta (hash-union (track-meta trk) meta
                                 #:combine left-biased)]))

;; =====================================

(module+ test
  (define P string->path)

  (define t1 (track* #:audio (P "a") #:output (P "A") (title: "aaa")))
  (define t2 (track* #:audio (P "b") #:output (P "B") (title: "bbb")))
  (define t3 (track* #:audio (P "c") #:output (P "C") (title: "ccc") (track-num: "4")))

  (check-equal?
   (with-output-to-string (λ () (display t2)))
   "#<track:\"bbb\">")

  (check-equal? (track-number t2) #f)
  (check-equal? (track-number t3) 4))

;; ---------------------------------------------------------------------------------------
;; Albums
;; --------------------

;; tracks : [listof track]
;; meta : [hasheq metadata-key => string]
(struct album [tracks meta]
  #:transparent
  #:extra-constructor-name make-album
  #:methods gen:custom-write
  [(define (write-proc alb port mode)
     (fprintf port "#<album:~s (~a track~a)>"
              (album-title alb)
              (length (album-tracks alb))
              (if ((list/c any/c) (album-tracks alb))
                "" "s")))])

;; (album title meta-or-tracks ...) : album
;; title : string
;; meta-or-tracks : (or metadata-entry track [listof track])
(define (album* title . xs)

  (define-values [meta tracks/rev]
    (for/fold ([meta (if title
                       (hasheq +album title)
                       (hasheq))]
               [tracks/rev '()])
              ([x (in-list xs)])
      (cond
        [(metadata-entry? x)
         (values (hash-set meta
                           (metadata-entry-key x)
                           (metadata-entry-value x))
                 tracks/rev)]

        [(track? x)
         (values meta (cons x tracks/rev))]

        [(list? x)
         (values meta (rev-append x tracks/rev))])))

  (define tracks
    (if (hash-empty? meta)
      (reverse tracks/rev)
      (for/fold ([ts '()]) ([t (in-list tracks/rev)])
        (cons (add-meta-to-track meta t) ts))))

  (make-album tracks meta))

;; track -> album
(define (album/single t)
  (album* #f t))

;; album -> string
(define (album-title a)
  (hash-ref (album-meta a) +album "(no title)"))

;; album -> [sequenceof track]
(define-syntax-rule (in-album-tracks a)
  (in-list (album-tracks a)))

;; =====================================

(module+ test

  (define t1+a (track* #:audio (P "a") #:output (P "A")
                       (title: "aaa") (album: "ABC")))
  (define t2+a (track* #:audio (P "b") #:output (P "B")
                       (title: "bbb") (album: "ABC")))
  (define t3+a (track* #:audio (P "c") #:output (P "C")
                       (title: "ccc") (track-num: "4") (album: "ABC") (artist: "Alphabet")))

  (define alb-1+2 (album* "ABC" t1 t2))
  (define alb-3+r (album* "ABC" t3 (artist: "Alphabet")))

  (check-equal?
   (with-output-to-string (λ () (display alb-1+2)))
   "#<album:\"ABC\" (2 tracks)>")

  (check-equal?
   (with-output-to-string (λ () (display (album/single t1))))
   "#<album:\"(no title)\" (1 track)>")

  (check-equal?
   alb-1+2
   (make-album (list t1+a t2+a)
               (hasheq +album "ABC")))

  (check-equal?
   alb-3+r
   (make-album (list t3+a)
               (hasheq +album "ABC"
                       +artist "Alphabet")))

  (check-equal?
   (album* "ABC" t1 t2)
   (album* "ABC" (list t1 t2)))

  (check-equal?
   (album* "ABC" t1 t2 t3)
   (album* "ABC" (list t1 t2) t3))

  (check-equal?
   (album* "ABC" t1 t2 t3)
   (album* "ABC" t1 (list t2 t3))))
