#lang racket/base
(provide
 recursively-delete-directory)

;; (recursively-delete-directory d) : void
;; d : path-string
(define (recursively-delete-directory d)
  (define leftover-dirs
    (for/fold ([l (list d)])
              ([p (in-directory d)])
      (if (directory-exists? p)
        (cons p l)
        (begin (delete-file p)
               l))))
  (for-each delete-directory leftover-dirs))
