#lang racket/base
(require
 "private/tracks-albums.rkt")

(module+ test
  (require rackunit racket/port racket/function))

;; ---------------------------------------------------------------------------------------
;; FFmpeg

;; (current-ffmpeg) : path
(define current-ffmpeg
  (make-parameter (build-path "/usr/bin/ffmpeg")))

;; (exec-ffmpeg cmdline-args) : exact-integer input-port input-port
;; cmdline-args : [listof string]
;; --
;; returns (values status-code stdout stderr)
(define (exec-ffmpeg args)
  (define-values [sp stdout stdin stderr]
    (apply subprocess
           (list* #f
                  #f
                  #f
                  (current-ffmpeg)
                  args)))
  (subprocess-wait sp)
  (values (subprocess-status sp)
          stdout
          stderr))

(module+ test
  (define FFMPEG-VERSION-REGEX
    #px"^ffmpeg version .* Copyright \\(c\\) .* the FFmpeg developers")

  (let-values ([(sc stdout stderr) (exec-ffmpeg '("-version"))])
    (check-equal? sc 0)
    (check-pred (curry regexp-match? FFMPEG-VERSION-REGEX)
                (port->string stdout))
    (check-equal? (port->string stderr) "")))

;; ---------------------------------------------------------------------------------------
;; Processing tracks

;; (current-output-directory) : path
(define current-output-directory
  (make-parameter (build-path "./musiclibrary")))
