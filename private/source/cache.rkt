#lang racket/base
(provide
 source-cache?)

(require
 "../source.rkt")

; TODO:
; ;; (current-cache-directory) : path
; (define current-cache-directory
;   (make-parameter (build-path "./musiclibrary-cache")))

;; source-cache ::= [hash source => path]
(define (source-cache? h)
  (and (hash? h)
       (for/and ([(k v) (in-hash h)])
         (and (source? k) (path? v)))))
