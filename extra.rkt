#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; sliced-tracks
 sliced-tracks)

(require
 "./private/tracks-albums.rkt"
 racket/vector
 syntax/parse/define
 (for-syntax racket/base
             "./private/time-value.rkt"))

(module+ test
  (require rackunit syntax/macro-testing))

;; ---------------------------------------------------------------------------------------
;; Utils
;; --------------------

;; (slice-timestamps stamps) : [listof X]
;; stamps : [listof (cons nat [(or nat #f) -> X])]
;; --
;; sorts each '(cons tm func)' in 'stamps', then returns the result of applying 'func' to
;; the next largest 'tm' in the list (or '#f' if it is the largest)
(define (slice-timestamps stamps_)
  (define stamps
    (sort stamps_ < #:key car))

  (if (null? stamps)
    '()
    (for/list ([stamp (in-list stamps)]
               [stamp* (in-list (append (cdr stamps) (list #f)))])
      (define func (cdr stamp))
      (func (and stamp* (car stamp*))))))

(module+ test
  (define (entry x)
    (cons x (λ (y) (list x y))))

  (check-equal? (slice-timestamps '()) '())

  (check-equal? (slice-timestamps
                 (list (entry 0)
                       (entry 1)
                       (entry 5)
                       (entry 2)))
                `([0 1]
                  [1 2]
                  [2 5]
                  [5 #f])))

;; ---------------------------------------------------------------------------------------
;; sliced-tracks
;; --------------------

(begin-for-syntax
  (define-syntax-class tv
    #:description "time value"
    [pattern x
             #:when (time-value? (syntax-e #'x))
             #:attr s #`#,(time-value->seconds (syntax-e #'x))
             #:attr ms #`#,(time-value->milliseconds (syntax-e #'x))])

  (define-splicing-syntax-class additional-arg
    #:attributes ([stuff 1])
    [pattern (x ...)
             #:with [stuff ...] #'[(x ...)]]
    [pattern {~seq k:keyword y}
             #:with [stuff ...] #'[k y]]))

(define-simple-macro (sliced-tracks #:audio src-expr
                                    {~seq title:str start:tv arg:additional-arg ...}
                                    ...)
  (let ([src src-expr])
    (slice-timestamps
     (list (cons 'start.s
                 (λ (end)
                   (track #:audio (audio-clip src start.s end)
                          ;; TODO: generate output somewhere else
                          #:output 'title
                          (title: 'title)
                          arg.stuff ... ...)))
           ...))))

;; =======================================================================================

(module+ test
  (define s (build-path "S"))
  (define i 0)

  (check-equal?
   (sliced-tracks #:audio (begin (set! i (add1 i)) s)
                  "b" 1:00.1
                  "a" 0:00 (artist: "A")
                  "c" 2:01)
   (list
    (track #:audio (audio-clip s 0 '1:00.1)
           #:output (build-path "a")
           (title: "a")
           (artist: "A"))
    (track #:audio (audio-clip s '1:00.1 '2:01)
           #:output (build-path "b")
           (title: "b"))
    (track #:audio (audio-clip s '2:01)
           #:output (build-path "c")
           (title: "c"))))

  (check-exn #px"sliced-tracks: expected time value"
             (λ () (convert-compile-time-error
                    (sliced-tracks #:audio s
                                   "foo" 1:))))

  ; side effects
  (check-equal? i 1))
