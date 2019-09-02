#lang racket/base
(provide (all-defined-out))

;; (plural n word [alt]) : string
;; n : nat
;; word, alt : string
(define (plural n word [alt (string-append word "s")])
  (if (= n 1) word alt))

;; (recursively-make-directory path) : void
;; path : path-string
(define (recursively-make-directory path)
  (let loop ([path (simplify-path path)])
    (define-values [root _final _must-be-dir?] (split-path path))
    (unless (directory-exists? path)
      (when (path? root)
        (loop root))
      (make-directory path))))
