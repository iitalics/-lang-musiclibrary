#lang racket/base
(require
 racket/set)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Albums and tracks
;; --------------------

;; album ::= [listof track]
(define (album? x)
  (and (list? x)
       (andmap track? x)))

;; track ::= (make-track src meta)
;; audio-src : source
;; meta : [hasheq symbol => string]
;; output-path : path
(struct track [audio-src
               meta
               output-path]
  #:extra-constructor-name mk-track
  #:methods gen:custom-write
  [(define (write-proc t port mode)
     (fprintf port "#<track:~a>" (track-title t)))])

;; source ::= path
(define source?
  path?)

;; track -> string
(define (track-title t)
  (hash-ref (track-meta t)
            'title
            "(no title)"))

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

;; ---------------------------------------------------------------------------------------

(module+ test
  (define t1 (mk-track (string->path "a") (hasheq 'title "A") (string->path "A")))
  (define t2 (mk-track (string->path "b") (hasheq 'title "B") (string->path "B")))
  (define t3 (mk-track (string->path "c") (hasheq 'title "C") (string->path "C")))

  (check-exn #px"not init"
             (λ () (current-tracks-to-generate*)))

  (check-equal?
   (with-generate-tracks
     (queue-track! t1)
     (queue-album! (album t2 t3))
     (queue-track! t3))
   (seteq t1 t2 t3)))
