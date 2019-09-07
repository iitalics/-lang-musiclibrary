#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; album
 album?
 in-album-tracks
 (contract-out
  [rename album* album
          (((or/c string? #f)) ; title
           (#:number? boolean?)
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
 track?
 in-track-metadata
 (contract-out
  [rename track* track
          ((#:audio audio-source?)
           (#:output path-string?)
           #:rest (listof metadata-entry?)
           . ->* . track?)]
  [track-audio-source (track? . -> . audio-source?)]
  [track-output-path (track? . -> . path?)]
  [track-metadata (track? metadata-key? . -> . (or/c string? #f))]
  [track-title (track? . -> . string?)]
  [track-number (track? . -> . (or/c exact-integer? #f))]))

(require
 "./time-value.rkt"
 "./source.rkt"
 "./metadata.rkt"
 (submod "./metadata.rkt" metadata-entry-struct)
 (only-in racket/list range)
 (only-in racket/hash hash-union))

(module+ test
  (require rackunit racket/match racket/port))

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
;; Tracks
;; --------------------

;; audio-source : audio-source
;; output-path : path
;; meta : (hasheq metadata-key => any)
(struct track [audio-source
               output-path
               meta]
  #:transparent
  #:extra-constructor-name make-track
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (fprintf port "#<track:~s>" (track-title t)))])

;; track metadata-key -> (or any #f)
(define (track-metadata trk k)
  (hash-ref (track-meta trk) k #f))

;; track -> [sequenceof metadata-entry]
(define-syntax-rule (in-track-metadata trk)
  (in-list (hash-map (track-meta trk) meta:)))

;; (track* #:audio audio-src [#:output out-path] m-e ...)
;; audio-src : audio-source
;; out-path : path-string
;; m-e : metadata-entry
(define (track** #:audio audio-src
                 #:output [out-path_ GENERATE-OUT-PATH]
                 . m-es)

  (define meta
    (for/hash ([m-e (in-list m-es)])
      (values (metadata-entry-key m-e)
              (metadata-entry-value m-e))))

  (define out-path
    (cond
      [(eq? out-path_ GENERATE-OUT-PATH)
       (build-path (hash-ref meta
                             +title
                             (λ ()
                               (error 'track
                                      "Cannot choose output path when no title is specified"))))]
      [else
       (build-path out-path_)]))

  (make-track audio-src out-path meta))

(define GENERATE-OUT-PATH
  (let () (struct gen-out-path []) (gen-out-path)))

(define track*
  (procedure-rename track** 'track))

;; track -> string
(define (track-title t)
  (or (track-metadata t +title)
      "(no title)"))

;; track-number -> (or nat #f)
(define (track-number t)
  (track-metadata t +track-num))

;; =====================================

(module+ test
  (define P build-path)

  (define t1 (track* #:audio (fs "a") #:output (P "A") (title: "aaa")))
  (define t2 (track* #:audio (make-audio-clip (fs "b") 1000 #f) #:output (P "B") (title: "bbb")))
  (define t3 (track* #:audio (fs "c") #:output (P "C") (title: "ccc") (track-num: 1)))

  (check-equal?
   (with-output-to-string (λ () (display t2)))
   "#<track:\"bbb\">")

  (check-equal? (track-number t2) #f)
  (check-equal? (track-number t3) 1)

  (check-equal?
   (track* #:audio (fs "a") (title: "the title"))
   (track* #:audio (fs "a") #:output (P "the title") (title: "the title")))

  (check-exn #px"no title"
             (λ ()
               (track* #:audio (fs "..."))))

  (check-match
   (for/list ([m (in-track-metadata t3)]) m)
   (list-no-order (== (title: "ccc"))
                  (== (track-num: 1)))))

;; ---------------------------------------------------------------------------------------
;; Track helpers
;; --------------------

;; [hasheq metadata-key => any] track -> track
(define (add-metas-to-track meta trk)
  (struct-copy track trk
               [meta (hash-union (track-meta trk) meta
                                 #:combine left-biased)]))

;; metadata-entry track -> track
(define (add-meta-to-track m-e trk)
  (struct-copy track trk
               [meta (hash-set (track-meta trk)
                               (metadata-entry-key m-e)
                               (metadata-entry-value m-e))]))

;; [listof track] -> [listof track]
(define (number-tracks trks)
  ; get available numbers: 1..n with existing track numbers removed
  (define nums
    (for/fold ([ns (range 1 (add1 (length trks)))])
              ([trk (in-list trks)])
      (cond
        [(track-number trk) => (λ (n) (remove n ns))]
        [else ns])))
  (map (λ (trk)
         (if (track-number trk)
           trk
           (begin0 (add-meta-to-track (track-num: (car nums)) trk)
             (set! nums (cdr nums)))))
       trks))

(module+ test

  (check-equal?
   (number-tracks (list t1 t2))
   (list (add-meta-to-track (track-num: 1) t1)
         (add-meta-to-track (track-num: 2) t2)))

  (check-equal?
   ; t3 already occupies track #1
   (number-tracks (list t1 t2 t3))
   (list (add-meta-to-track (track-num: 2) t1)
         (add-meta-to-track (track-num: 3) t2)
         t3)))

;; ---------------------------------------------------------------------------------------
;; Albums
;; --------------------

;; tracks : [listof track]
;; meta : [hasheq metadata-key => any]
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

;; (album title [#:number? num?] meta-or-tracks ...) : album
;; title : string
;; num? : boolean
;; meta-or-tracks : (or metadata-entry track [listof track])
(define (album* title #:number? [num? #t] . xs)

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

  (let* ([tracks (reverse tracks/rev)]
         [tracks (if (hash-empty? meta)
                   tracks
                   (map (λ (t) (add-metas-to-track meta t)) tracks))]
         [tracks (if num?
                   (number-tracks tracks)
                   tracks)])

    (make-album tracks meta)))

;; track -> album
(define (album/single trk)
  (album* #f #:number? #f trk))

;; album -> string
(define (album-title a)
  (hash-ref (album-meta a) +album "(no title)"))

;; album -> [sequenceof track]
(define-syntax-rule (in-album-tracks a)
  (in-list (album-tracks a)))

;; =====================================

(module+ test

  (define t1+a (track* #:audio (fs "a") #:output (P "A")
                       (title: "aaa") (album: "ABC")))
  (define t2+a (track* #:audio (make-audio-clip (fs "b") 1000 #f) #:output (P "B")
                       (title: "bbb") (album: "ABC")))
  (define t3+a (track* #:audio (fs "c") #:output (P "C")
                       (title: "ccc") (track-num: 1) (album: "ABC") (artist: "Alphabet")))

  (define alb-1+2 (album* "ABC" t1 t2 #:number? #f))
  (define alb-1+2/n (album* "ABC" t1 t2)) ; #:number? #t
  (define alb-3+r (album* "ABC" t3 (artist: "Alphabet") #:number? #f))

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
   alb-1+2/n
   (make-album (number-tracks (list t1+a t2+a))
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
