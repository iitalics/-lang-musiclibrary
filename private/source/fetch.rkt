#lang racket/base
(require racket/contract)

(provide
 (contract-out
  (struct (exn:fail:fetch exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [reason exn?]))
  [source-fetch (source? . -> . path?)]))

(require
 "../source.rkt"
 (submod "../source.rkt" source-constructor-patterns)
 racket/match
 racket/path
 racket/function)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; exn:fail:fetch
;; --------

;; (make-exn:fail:fetch msg cmarks reason)
;; reason : exn
(struct exn:fail:fetch exn:fail [reason]
  #:extra-constructor-name make-exn:fail:fetch)

;; source exn -> !
(define (reraise-fetch-exn src err)
  (define msg (format "failed to fetch source: ~s\n  reason: ~a" src (exn-message err)))
  (raise (make-exn:fail:fetch msg (current-continuation-marks) err)))

;; ---------------------------------------------------------------------------------------
;; source-feth
;; --------

;; (source-fetch src) : path
;; src : source
;; ---
;; raises 'exn:fail:fetch' on error
(define (source-fetch src)
  (with-handlers ([exn:fail:fetch? raise]
                  [exn:fail? (curry reraise-fetch-exn src)])
    (match src
      [(source:fs path_)
       ; TODO: search multiple directories for the absolute path?
       (define path (normalize-path path_))
       (unless (file-exists? path)
         (define msg (format "source file ~s not found" (path->string path)))
         (raise (make-exn:fail:filesystem msg (current-continuation-marks))))
       path])))

;; ==========================================

(module+ test
  (check-equal? (source-fetch (fs "../../example/lain.png"))
                (normalize-path (build-path "../../example/lain.png")))

  (for ([bad-path (in-list '("../../example/doesnt-exist.png"
                             "../../example"))])
    (check-exn (λ (e) (and (exn:fail:fetch? e)
                           (exn:fail:filesystem? (exn:fail:fetch-reason e))
                           (regexp-match? #px"reason: source file .* not found"
                                          (exn-message e))))
               (λ () (source-fetch (fs bad-path))))))
