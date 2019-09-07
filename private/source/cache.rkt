#lang racket/base
(require racket/contract)

(provide
 source-cache?
 (contract-out)
 ; ---
 ; exn:fail:source-cache-miss
 (contract-out
  (struct (exn:fail:source-cache-miss exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [source source?]))
  ; ---
  ; source-cache
  [empty-source-cache source-cache?]
  [source-in-cache? (source-cache? source? . -> . any)]
  [source-cache-ref (source-cache? source? . -> . path?)]
  [source-cache-add (source-cache? source? path? . -> . source-cache?)]))

(require
 "../source.rkt"
 "./fetch.rkt")

(module+ test
  (require rackunit racket/path))

;; ---------------------------------------------------------------------------------------

(module cache-miss-exn racket/base
  (provide (all-defined-out))

  ;; (exn:fail:source-cache-miss ... source)
  ;; source : source
  (struct exn:fail:source-cache-miss exn:fail
    [source])

  ;; source -> !
  (define (raise-source-cache-miss src)
    (raise (exn:fail:source-cache-miss (format "source needed: ~a" src)
                                       (current-continuation-marks)
                                       src))))

(module test-utils racket/base
  (provide (all-defined-out))
  (require (submod ".." cache-miss-exn))

  (define ((cache-miss= src) e)
    (and (exn:fail:source-cache-miss? e)
         (equal? (exn:fail:source-cache-miss-source e) src))))

(require 'cache-miss-exn)
(module+ test (require (submod ".." test-utils)))

;; ---------------------------------------------------------------------------------------

;; source-cache ::= [hash source => path]
(define (source-cache? h)
  (and (hash? h)
       (for/and ([(k v) (in-hash h)])
         (and (source? k) (path? v)))))

;; source-cache?
(define empty-source-cache #hash())

;; (source-in-cache? cache src) : boolean
;; cache : source-cache
;; src : source
(define (source-in-cache? cache src)
  (hash-has-key? cache src))

;; (source-cache-ref cache src fail) : path
;; cache : source-cache
;; src : source
;; --
;; raises exn:fail:source-cache-miss is 'src' not in cache
(define (source-cache-ref cache src)
  (hash-ref cache src (λ () (raise-source-cache-miss src))))

;; (source-cache-ref cache src path) : source-cache
;; cache : source-cache
;; src : source
;; path : path
(define (source-cache-add cache src path)
  (hash-update cache src values path))

;; =======================================================================================

(module+ test
  (define lain-src (fs "../../example/lain.png"))
  (define audio-src (fs "../../example/test-audio.ogg"))

  (define sc0 empty-source-cache)
  (define sc1 (source-cache-add sc0 lain-src (build-path "A")))
  (define sc2 (source-cache-add sc1 audio-src (build-path "B")))

  (check-exn (cache-miss= lain-src) (λ () (source-cache-ref sc0 lain-src)))
  (check-exn (cache-miss= audio-src) (λ () (source-cache-ref sc0 audio-src)))
  (check-exn (cache-miss= audio-src) (λ () (source-cache-ref sc1 audio-src)))
  (check-equal? (source-cache-ref sc1 lain-src) (build-path "A"))
  (check-equal? (source-cache-ref sc2 audio-src) (build-path "B")))
