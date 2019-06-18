#lang racket
(provide
 (rename-out [module-begin #%module-begin])

 (except-out (all-from-out racket)
             #%module-begin)

 (except-out (all-from-out musiclibrary)
             musiclibrary))

(require
 musiclibrary)

;; ---------------------------------------------------------------------------------------

(define-syntax-rule (module-begin body ...)
  (#%plain-module-begin
   (musiclibrary
    body ...)))

;; ---------------------------------------------------------------------------------------

(module reader syntax/module-reader
  musiclibrary/lang)
