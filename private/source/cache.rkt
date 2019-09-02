#lang racket/base
(require racket/contract)

(provide
 source-cache?
 (contract-out
  [empty-source-cache source-cache?]
  [source-in-cache? (source-cache? source? . -> . any)]
  [source-cache-ref ((source-cache? source?) [(source? . -> . any)] . ->* . any)]
  [source-cache-add (source-cache? source? path? . -> . source-cache?)]))

(require
 "../source.rkt"
 "./fetch.rkt")

(module+ test
  (require rackunit racket/path))

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

;; (source-cache-ref cache src fail) : (or path T)
;; cache : source-cache
;; src : source
;; fail : [source -> T]
(define (source-cache-ref cache src [fail values])
  (hash-ref cache src (λ () (fail src))))

;; (source-cache-ref cache src path) : source-cache
;; cache : source-cache
;; src : source
;; path : path
(define (source-cache-add cache src path)
  (hash-update cache src values path))

;; =======================================================================================

(module+ test
  (define (-fail- _src) 'couldnt-find)

  (define lain-src (fs "../../example/lain.png"))
  (define audio-src (fs "../../example/test-audio.ogg"))

  (define sc0 empty-source-cache)
  (check-equal? (source-cache-ref sc0 lain-src -fail-) 'couldnt-find)
  (check-equal? (source-cache-ref sc0 audio-src -fail-) 'couldnt-find)

  (define sc1 (source-cache-add sc0 lain-src (build-path "A")))
  (define sc2 (source-cache-add sc1 audio-src (build-path "B")))
  (check-equal? (source-cache-ref sc1 lain-src -fail-) (build-path "A"))
  (check-equal? (source-cache-ref sc1 audio-src -fail-) 'couldnt-find)
  (check-equal? (source-cache-ref sc2 audio-src -fail-) (build-path "B")))
