#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; exn:fail:ffmpeg
 (contract-out
  (struct (exn:fail:ffmpeg exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [status-code exact-integer?]
     [stdout input-port?]
     [stderr input-port?])))
 ; ---
 ; exec-ffmpeg
 (contract-out
  [current-ffmpeg (parameter/c path?)]
  [exec-ffmpeg ([listof (or/c string?
                              bytes?
                              path?)]
                . -> .
                (values input-port? input-port?))]))

(module+ test
  (require rackunit racket/port racket/function))

;; ---------------------------------------------------------------------------------------

;; (current-ffmpeg) : path
(define current-ffmpeg
  (make-parameter (build-path "/usr/bin/ffmpeg")))

;; (exn:fail:ffmpeg ... status-code stdout stderr)
;; status-code : (and exact-integer (not zero))
;; stdout, stderr : input-port
(struct exn:fail:ffmpeg exn:fail [status-code stdout stderr])

;; (exec-ffmpeg cmdline-args) : input-port input-port
;; cmdline-args : [listof string]
;; --
;; returns (values status-code stdout stderr)
;; raises exn:fail:ffmpeg if process returns non-zero status
(define (exec-ffmpeg args)
  (define-values [sp stdout stdin stderr]
    (apply subprocess
           (list* #f
                  #f
                  #f
                  (current-ffmpeg)
                  args)))
  (subprocess-wait sp)
  (define sc (subprocess-status sp))
  (if (zero? sc)
    (values stdout stderr)
    (raise (exn:fail:ffmpeg (format "ffmpeg failed with status code: ~a" sc)
                            (current-continuation-marks)
                            sc
                            stdout
                            stderr))))

;; ---------------------------------------------------------------------------------------

(module+ test
  (define FFMPEG-VERSION-REGEX
    #px"^ffmpeg version .* Copyright \\(c\\) .* the FFmpeg developers")

  (let-values ([(stdout stderr) (exec-ffmpeg '("-version"))])
    (check-pred (curry regexp-match? FFMPEG-VERSION-REGEX)
                (port->string stdout))
    (check-equal? (port->string stderr) ""))

  (check-exn (λ (e)
               (and (exn:fail:ffmpeg? e)
                    (regexp-match #px"Unrecognized option 'notathing'"
                                  (port->string
                                   (exn:fail:ffmpeg-stderr e)))))
             (λ ()
               (exec-ffmpeg '("-notathing")))))
