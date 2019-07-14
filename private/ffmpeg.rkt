#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; exn:fail:ffmpeg
 (contract-out
  (struct (exn:fail:ffmpeg exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [args (listof string?)]
     [status-code exact-integer?]
     [stdout input-port?]
     [stderr input-port?])))
 ; ---
 ; args
 ffmpeg-args?
 (contract-out
  [make-ffmpeg-args (path? . -> . ffmpeg-args?)]
  [ffmpeg-args-add-input (ffmpeg-args? path? (listof string?) . -> . ffmpeg-args?)]
  [ffmpeg-args-set-metadata (ffmpeg-args? symbol? string? . -> . ffmpeg-args?)])
 ; ---
 ; exec-ffmpeg
 (contract-out
  [current-ffmpeg (parameter/c path?)]
  [exec-ffmpeg (ffmpeg-args? . -> . (values input-port? input-port?))]))

(require
 racket/format)

(module+ test
  (require
   rackunit
   threading
   racket/port
   racket/function))

;; ---------------------------------------------------------------------------------------

;; (current-ffmpeg) : path
(define current-ffmpeg
  (make-parameter (build-path "/usr/bin/ffmpeg")))

;; (exn:fail:ffmpeg ... status-code stdout stderr)
;; status-code : (and exact-integer (not zero))
;; stdout, stderr : input-port
(struct exn:fail:ffmpeg exn:fail
  [args status-code stdout stderr])

;; ---------------------------------------------------------------------------------------
;; Args
;; ----------

;; overwrite? : boolean
;; output-path : path
;; metadata : [listof (cons symbol string)]
;; inputs : [listof [listof string]]
(struct ffmpeg-args [overwrite? output-path metadata inputs]
  #:transparent
  #:constructor-name mk-ffmpeg-args)

(define (make-ffmpeg-args output-path)
  (mk-ffmpeg-args #t output-path '() '()))

;; (ffmpeg-args-set-metadata f-a k v) : ffmpeg-args
;; f-a : ffmpeg-args
;; k : symbol
;; v : string
(define (ffmpeg-args-set-metadata f-a k v)
  (struct-copy ffmpeg-args f-a
    [metadata (cons (cons k v) (ffmpeg-args-metadata f-a))]))

;; (ffmpeg-args-add-input f-a inp-path inp-flags) : ffmpeg-args
;; f-a : ffmpeg-args
;; inp-path : path?
;; inp-flags : [listof string]
(define (ffmpeg-args-add-input f-a inp-path inp-flags)
  (struct-copy ffmpeg-args f-a
    [inputs (append (ffmpeg-args-inputs f-a)
                    (list `(,@inp-flags ,"-i" ,(path->string inp-path))))]))

;; ffmpeg-args -> [listof string]
(define (ffmpeg-args->strings f-a)
  `(,@(if (ffmpeg-args-overwrite? f-a) '("-y") '())
    ; input sources
    ,@(apply append (ffmpeg-args-inputs f-a))
    ; metdata
    "-map_metadata" "-1" ; (prevents metadata from being copied over from input streams)
    ,@(for/fold ([args '()])
                ([k/v (in-list (ffmpeg-args-metadata f-a))])
        (list* "-metadata:g" (format "~a=~a" (car k/v) (cdr k/v)) args))
    ; output
    ,(path->string (ffmpeg-args-output-path f-a))))

(module+ test
  (check-equal? (~> (make-ffmpeg-args (build-path "OUT"))
                    ffmpeg-args->strings)
                '("-y"
                  "-map_metadata" "-1"
                  "OUT"))

  (check-equal? (~> (make-ffmpeg-args (build-path "OUT"))
                    (ffmpeg-args-add-input _ (build-path "SRC") '("-ss" "100"))
                    ffmpeg-args->strings)
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "OUT"))

  (check-equal? (~> (make-ffmpeg-args (build-path "OUT"))
                    (ffmpeg-args-add-input _ (build-path "SRC") '("-ss" "100"))
                    (ffmpeg-args-set-metadata _ 'foo "x")
                    ffmpeg-args->strings)
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "-metadata:g" "foo=x"
                  "OUT"))

  (check-equal? (~> (make-ffmpeg-args (build-path "OUT"))
                    (ffmpeg-args-add-input _ (build-path "SRC") '("-ss" "100"))
                    (ffmpeg-args-set-metadata _ 'foo "x")
                    (ffmpeg-args-set-metadata _ 'bar "y")
                    ffmpeg-args->strings)
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "-metadata:g" "foo=x" "-metadata:g" "bar=y"
                  "OUT")))

;; ---------------------------------------------------------------------------------------
;; Exec
;; ----------

;; (exec-ffmpeg/raw args) : input-port input-port
;; (exec-ffmpeg fa) : input-port input-port
;; args : [listof string]
;; fa : ffmpeg-args
;; --
;; returns (values stdout stderr)
;; raises exn:fail:ffmpeg if process returns non-zero status
(define (exec-ffmpeg/raw args)

  (define-values [sp stdout stdin stderr]
    (apply subprocess
           (list* #f
                  #f
                  #f
                  (current-ffmpeg)
                  args)))

  (define sc
    (begin (subprocess-wait sp)
           (subprocess-status sp)))

  (unless (zero? sc)
    (define msg
      (~a "ffmpeg failed with status code: " sc
          "\n  arguments:"
          (for/fold ([acc ""])
                    ([a (in-list args)])
            (~a acc " " a))))

    (raise (exn:fail:ffmpeg msg
                            (current-continuation-marks)
                            args
                            sc
                            stdout
                            stderr)))
  (values stdout stderr))

(define (exec-ffmpeg fa)
  (exec-ffmpeg/raw (ffmpeg-args->strings fa)))

;; =======================================================================================

(module+ test
  (define FFMPEG-VERSION-REGEX
    #px"^ffmpeg version .* Copyright \\(c\\) .* the FFmpeg developers")

  (let-values ([(stdout stderr) (exec-ffmpeg/raw '("-version"))])
    (check-pred (curry regexp-match? FFMPEG-VERSION-REGEX) (port->string stdout))
    (check-equal? (port->string stderr) ""))

  (check-exn (λ (e)
               (and (exn:fail:ffmpeg? e)
                    (regexp-match #px"Unrecognized option 'notathing'"
                                  (port->string
                                   (exn:fail:ffmpeg-stderr e)))))
             (λ ()
               (exec-ffmpeg/raw '("-notathing")))))
