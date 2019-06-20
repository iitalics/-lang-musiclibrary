#lang racket/base
(require racket/contract)

(provide
 time-value?
 (contract-out
  [time-value->milliseconds
   (time-value? . -> . exact-nonnegative-integer?)]))

(require
 racket/match
 (for-syntax racket/base))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; time-value

(define TIME-VALUE-REGEXP
  ; tv ::= [d]d:dd:dd[.d+] | [d]d:dd[.d+]
  #px"^((\\d{1,2}):(\\d{2}):(\\d{2})|(\\d{1,2}):(\\d{2}))(.\\d+)?$")

(define (time-value? x)
  (or (and (real? x)
           (not (negative? x)))
      (and (string? x)
           (regexp-match? TIME-VALUE-REGEXP x))
      (and (symbol? x)
           (regexp-match? TIME-VALUE-REGEXP
                          (symbol->string x)))))

;; float -> nat
(define (s->ms x)
  (inexact->exact (round (* 1000 x))))

;; time-value -> nat
(define (time-value->milliseconds x)
  (cond
    [(number? x) (s->ms x)]
    [(symbol? x) (parse-time (symbol->string x))]
    [else        (parse-time x)]))

;; string (matching TIME-VALUE-REGEXP) -> nat
(define (parse-time str)
  (define-values [hr mn sc frac]
    (match (regexp-match TIME-VALUE-REGEXP str)
      [(list _ _ #f #f #f mn sc frac)
       (values "0" mn sc (or frac "0"))]
      [(list _ _ hr mn sc #f #f frac)
       (values hr mn sc (or frac "0"))]))

  (+ (* 1000 60 60 (string->number hr))
     (* 1000 60    (string->number mn))
     (* 1000       (string->number sc))
     (s->ms (string->number frac))))

(module+ test

  (check-equal? (time-value->milliseconds '0.5)
                500)
  (check-equal? (time-value->milliseconds '1/3)
                333)
  (check-equal? (time-value->milliseconds '20)
                (* 1000 20))
  (check-equal? (time-value->milliseconds '12:20)
                (* 1000 (+ (* 12 60) 20)))
  (check-equal? (time-value->milliseconds '1:23:20.5)
                (+ 500 (* 1000 (+ (* 1 60 60) (* 23 60) 20))))
  (check-equal? (time-value->milliseconds '13:45:17)
                (* 1000 (+ (* 13 60 60) (* 45 60) 17)))

  (check-false (time-value? "1:5"))
  (check-false (time-value? "12:3:45"))
  (check-false (time-value? "123"))
  (check-false (time-value? ".5")))
