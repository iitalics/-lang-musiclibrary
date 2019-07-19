#lang racket/base
(require racket/contract)

(provide
 source-cache?
 (contract-out
  [empty-source-cache source-cache?]
  [source-in-cache? (source-cache? source? . -> . any)]
  [source-cache ((source-cache? source?) [#:fetch (source? . -> . path?)] . ->* . source-cache?)]
  [source-cache-ref ((source-cache? source?) [(source? . -> . any)] . ->* . any)]))

(require
 "../source.rkt"
 "./fetch.rkt")

(module+ test
  (require rackunit racket/path))

;; ---------------------------------------------------------------------------------------

; TODO:
; ;; (current-cache-directory) : path
; (define current-cache-directory
;   (make-parameter (build-path "./musiclibrary-cache")))

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

;; (source-cache cache src #:fetch [fetch]) : source-cache
;; cache : source-cache
;; src : source
;; fetch : [source -> path]
(define (source-cache cache src #:fetch [fetch-fn source-fetch])
  (if (source-in-cache? cache src)
    (error 'source-cache
           (format "BUG: doesn't realize this source was already cached: ~s" src))
    (let ([resolved-path (fetch-fn src)])
      (hash-set cache src resolved-path))))

;; =======================================================================================

(module+ test
  (define (-fail- _src) 'couldnt-find)

  (define lain-src (fs "../../example/lain.png"))
  (define audio-src (fs "../../example/test-audio.ogg"))

  (define c0 empty-source-cache)
  (check-equal? (source-cache-ref c0 lain-src -fail-) 'couldnt-find)
  (check-equal? (source-cache-ref c0 audio-src -fail-) 'couldnt-find)

  ; NOTE: these do IO!
  (define c1 (source-cache c0 lain-src))
  (define c2 (source-cache c1 audio-src))
  (check-equal? (source-cache-ref c1 lain-src -fail-)
                (normalize-path "../../example/lain.png"))
  (check-equal? (source-cache-ref c1 audio-src -fail-)
                'couldnt-find)
  (check-equal? (source-cache-ref c2 audio-src -fail-)
                (normalize-path "../../example/test-audio.ogg"))

  (for ([x (in-list `([,c0 ,lain-src  ,#f]
                      [,c0 ,audio-src ,#f]
                      [,c1 ,lain-src  ,#t]
                      [,c1 ,audio-src ,#f]
                      [,c2 ,lain-src  ,#t]
                      [,c2 ,audio-src ,#t]))])
    (check-equal? (source-in-cache? (car x) (cadr x)) (caddr x))))
