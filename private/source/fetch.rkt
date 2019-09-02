#lang racket/base
(require racket/contract)

(provide
 (contract-out
  (struct (exn:fail:fetch exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [reason exn?]))
  [source-fetch (source? . -> . path?)]
  [current-local-cache-directory (parameter/c path?)]))

(require
 "../utils.rkt"
 "../source.rkt"
 (submod "../source.rkt" source-constructor-patterns)
 racket/match
 racket/path
 racket/function
 (only-in net/url get-pure-port)
 (only-in racket/port copy-port))

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
;; source->local-cache-path
;; --------

;; source -> path
(define (source->local-cache-path src)
  (build-path (current-local-cache-directory)
              (format "~a~a"
                      (source-hash-string src)
                      (source-extension src))))

;; (current-cache-directory) : path
(define current-local-cache-directory
  (make-parameter (build-path "./musiclibrary-cache")))

;; (call/output-to-local-cache src f) : path
;; src : source
;; f : -> void
(define (call/output-to-local-cache src f)
  (define path (source->local-cache-path src))
  (define-values [dir _file _dir?] (split-path path))
  (recursively-make-directory dir)
  (with-output-to-file path #:exists 'replace f)
  path)

(define-syntax-rule (with-output-to-local-cache src body ...)
  (call/output-to-local-cache src (λ () body ...)))

(module+ test
  (parameterize ([current-local-cache-directory (build-path "./MLC")])
    (for ([x (in-list `([,(fs "foo.png") "./MLC/~a.png"]
                        [,(net "https://a.com/b/c.jpg") "./MLC/~a.jpg"]
                        [,(fs "bar") "./MLC/~a"]))])
      (match-define `[,src ,fmt] x)
      (check-equal? (source->local-cache-path src)
                    (build-path (format fmt (source-hash-string src)))))))

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
       path]

      [(source:net url)
       (with-output-to-local-cache src
         ;; TODO: http connection pool?
         (define conn-port (get-pure-port url))
         (copy-port conn-port (current-output-port))
         (close-input-port conn-port))])))

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
