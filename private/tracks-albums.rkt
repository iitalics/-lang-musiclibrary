#lang racket/base
(require racket/contract)

(provide
 source? metadata? album? track?
 ; ---
 ; album
 (contract-out
  [album (() #:rest (listof track?) . ->* . album?)])
 ; ---
 ; track
 track-audio-src
 track-meta
 track-output-path
 (contract-out
  [rename track*
          track
          ((#:audio-src source?
            #:output-path path-string?)
           #:rest any/c . ->* . track?)]
  [make-track (source? metadata? path? . -> . track?)]
  [track-title (track? . -> . string?)])
 ; ---
 ; current-tracks-to-generate
 (contract-out
  [rename current-tracks-to-generate*
          current-tracks-to-generate
          (-> (set/c track?))]
  [queue-track! (track? . -> . any)]
  [queue-album! (album? . -> . any)])
 with-generate-tracks)

(require
 racket/set)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Albums and tracks
;; --------------------

;; source ::= path
(define (source? x)
  (path? x))

;; metadata ::= [hasheq symbol => string]
(define (metadata? x)
  (and (hash? x)
       (for/and ([(k v) (in-hash x)])
         (and (symbol? k) (string? v)))))

;; album ::= [listof track]
(define (album? x)
  (and (list? x)
       (andmap track? x)))

;; track ::= (make-track src meta)
;; audio-src : source
;; meta : metadata
;; output-path : path
(struct track [audio-src
               meta
               output-path]
  #:extra-constructor-name make-track
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (fprintf port "#<track:~a>" (track-title t)))])

;; track -> string
(define (track-title t)
  (hash-ref (track-meta t)
            'title
            "(no title)"))

;; ... -> track
(define (track* #:audio-src as
                #:output-path op
                . meta-args)
  (make-track as
              (apply hasheq meta-args)
              (build-path op)))

;; track ... -> album
(define (album . ts)
  ts)

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

;; [A ... -> any] A ... -> [seteqof tracks]
(define (call/generate-tracks f . args)
  (parameterize ([current-tracks-to-generate (seteq)])
    (apply f args)
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
  (define t1 (make-track (string->path "a") (hasheq 'title "A") (string->path "A")))
  (define t2 (make-track (string->path "b") (hasheq 'title "B") (string->path "B")))
  (define t3 (make-track (string->path "c") (hasheq 'title "C") (string->path "C")))

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
