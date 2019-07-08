#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; exn:fail:ffmpeg
 (contract-out
  (struct (exn:fail:ffmpeg exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [args (listof (or/c string? bytes? path?))]
     [status-code exact-integer?]
     [stdout input-port?]
     [stderr input-port?])))
 ; ---
 ; args
 ffmpeg-args?
 (contract-out
  [make-ffmpeg-args ((path?
                      #:asrc-path path?
                      #:asrc-flags (listof string?))
                     . ->* . ffmpeg-args?)]
  [ffmpeg-args-set-metadata (ffmpeg-args? symbol? string? . -> . ffmpeg-args?)])
 ; ---
 ; exec-ffmpeg
 (contract-out
  [current-ffmpeg (parameter/c path?)]
  [exec-ffmpeg (ffmpeg-args? . -> . (values input-port? input-port?))]))

(require
 racket/format)

(module+ test
  (require rackunit racket/port racket/function))

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
;; asrc-path : path
;; asrc-flags : [listof string]
(struct ffmpeg-args [overwrite? output-path metadata asrc-path asrc-flags]
  #:transparent
  #:constructor-name mk-ffmpeg-args)

(define (make-ffmpeg-args output-path
                          #:asrc-path asrc-path
                          #:asrc-flags asrc-flags)
  (mk-ffmpeg-args #t output-path '() asrc-path asrc-flags))

;; (ffmpeg-args-set-metadata as k v) : ffmpeg-args
;; as : ffmpeg-args
;; k : symbol
;; v : string
(define (ffmpeg-args-set-metadata as k v)
  (struct-copy ffmpeg-args as
    [metadata (cons (cons k v) (ffmpeg-args-metadata as))]))

;; ffmpeg-args -> [listof string]
(define (ffmpeg-args->strings as)
  `(,@(if (ffmpeg-args-overwrite? as) '("-y") '())
    ; audio src
    ,@(ffmpeg-args-asrc-flags as)
    "-i" ,(path->string (ffmpeg-args-asrc-path as))
    ; metdata
    "-map_metadata" "-1" ; (prevents metadata from being copied over from input streams)
    ,@(for/fold ([args '()])
                ([k/v (in-list (ffmpeg-args-metadata as))])
        (list* "-metadata:g" (format "~a=~a" (car k/v) (cdr k/v)) args))
    ; output
    ,(path->string (ffmpeg-args-output-path as))))

(module+ test
  (check-equal? (ffmpeg-args->strings
                 (make-ffmpeg-args (build-path "OUT")
                                   #:asrc-path (build-path "SRC")
                                   #:asrc-flags '("-ss" "100")))
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "OUT"))

  (check-equal? (ffmpeg-args->strings
                 (ffmpeg-args-set-metadata
                  (make-ffmpeg-args (build-path "OUT")
                                    #:asrc-path (build-path "SRC")
                                    #:asrc-flags '("-ss" "100"))
                  'foo "x"))
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "-metadata:g" "foo=x"
                  "OUT"))

  (check-equal? (ffmpeg-args->strings
                 (ffmpeg-args-set-metadata
                  (ffmpeg-args-set-metadata
                   (make-ffmpeg-args (build-path "OUT")
                                     #:asrc-path (build-path "SRC")
                                     #:asrc-flags '("-ss" "100"))
                   'foo "x")
                  'bar "y"))
                '("-y"
                  "-ss" "100" "-i" "SRC"
                  "-map_metadata" "-1"
                  "-metadata:g" "foo=x" "-metadata:g" "bar=y"
                  "OUT")))

;; ---------------------------------------------------------------------------------------
;; Exec
;; ----------

;; (exec-ffmpeg/raw cmdline-args) : input-port input-port
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
