#lang racket/base
(require racket/contract)

(provide
 (contract-out
  [generate-music-library ((listof album?) . -> . void?)]))

(require
 "./tracks-albums.rkt"
 "./process-track.rkt")

;; ---------------------------------------------------------------------------------------
;; Entry point
;; --------------------

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)
  (recursively-make-directory (current-output-directory))
  (for* ([a (in-list library)]
         [t (in-album-tracks a)])
    (printf "Processing: ~a\n" t)
    (process-track t)))

;; (recursively-make-directory path) : void
;; path : path-string
(define (recursively-make-directory path)
  (let loop ([path (simplify-path path)])
    (define-values [root _final _must-be-dir?] (split-path path))
    (unless (directory-exists? path)
      (when (path? root)
        (loop root))
      (make-directory path))))
