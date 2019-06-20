#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; sliced-tracks
 sliced-tracks
 (contract-out
  [make-sliced-tracks
   (source?
    (listof (cons/c string? exact-nonnegative-integer?))
    . -> . (listof track?))]))

(require
 "./private/tracks-albums.rkt"
 racket/vector
 syntax/parse/define
 (for-syntax racket/base
             "./private/time-value.rkt"))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; sliced-tracks
;; --------------------

(begin-for-syntax
  (define-syntax-class tv
    [pattern {~and x {~or :number :id}}
             #:when (time-value? (syntax-e #'x))
             #:attr ms #`#,(time-value->milliseconds (syntax-e #'x))]))

(define-simple-macro (sliced-tracks #:audio src-expr
                                    {~seq title:str start:tv}
                                    ...)
  (make-sliced-tracks src-expr
                      '([title . start.ms]
                        ...)))

;; (make-sliced-tracks src stamps) : [listof track]
;; src : source
;; stamps : [listof (cons string nat)]
(define (make-sliced-tracks src stamps)
  (define stamps* (list->vector stamps))
  (vector-sort! stamps* < #:key cdr)
  (for/list ([i (in-naturals)]
             [stamp (in-vector stamps*)])
    (define title (car stamp))
    (define start/ms (cdr stamp))
    (define end/ms (if (= (add1 i) (vector-length stamps*))
                     #f
                     (cdr (vector-ref stamps* (add1 i)))))
    (track
     #:audio (make-audio-clip src start/ms end/ms)
     ; TODO: get around having to supply this argument
     #:output (build-path title)
     (title: title))))

;; =======================================================================================

(module+ test
  (define s (build-path "S"))

  ;; -----------
  ;; make-sliced-tracks

  (check-equal? (make-sliced-tracks s '()) '())

  (check-equal?
   (make-sliced-tracks s '(["a" . 0]
                           ["b" . 100]
                           ["c" . 200]))
   (list (track #:audio (audio-clip s 0 0.1)
                #:output (build-path "a")
                (title: "a"))
         (track #:audio (audio-clip s 0.1 0.2)
                #:output (build-path "b")
                (title: "b"))
         (track #:audio (audio-clip s 0.2)
                #:output (build-path "c")
                (title: "c"))))

  ; commutative
  (check-equal? (make-sliced-tracks s '(["a" . 0]
                                        ["b" . 200]
                                        ["c" . 500]))
                (make-sliced-tracks s '(["a" . 0]
                                        ["c" . 500]
                                        ["b" . 200])))

  ;; -----------
  ;; sliced-tracks

  (define i 0)

  (check-equal?
   (sliced-tracks #:audio (begin (set! i (add1 i)) s)
                  "e" 0:00
                  "g" 1:00.1
                  "h" 2:01)
   (make-sliced-tracks s '(["e" .      0]
                           ["g" .  60100]
                           ["h" . 121000])))

  ; side effects
  (check-equal? i 1))
