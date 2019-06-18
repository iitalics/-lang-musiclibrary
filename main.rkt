#lang racket/base
(require
 "private/tracks-albums.rkt")

(module+ test
  (require rackunit racket/port racket/function))

;; ---------------------------------------------------------------------------------------
;; FFmpeg
;; --------------------

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

  (let-values ([(code stdout stderr) (exec-ffmpeg '("-version"))])
    (check-equal? code 0)
    (check-pred (curry regexp-match? FFMPEG-VERSION-REGEX)
                (port->string stdout))
    (check-equal? (port->string stderr) "")))

;; ---------------------------------------------------------------------------------------
;; Processing tracks
;; --------------------

;; (current-output-directory) : path
(define current-output-directory
  (make-parameter (build-path "./musiclibrary")))

;; (process-track trk) : void
;; trk : track
(define (process-track trk)
  (define-values [code _stdout _stderr]
    (exec-ffmpeg `("-y"
                   "-i"
                   ,(track-audio-src trk)
                   ,(build-path (current-output-directory)
                                (track-output-path trk)))))
  (unless (zero? code)
    (error 'process-track
           (format "ffmpeg returned status code: ~a" code))))

;; =======================================================================================

(module+ test

  ;; -----------------------------------
  ;; Utils

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

  ;; [-> A] -> A
  (define (call/test-musiclibrary f)
    (parameterize ([current-output-directory
                    (build-path "./test-musiclibrary")])
      (define (del)
        (when (directory-exists? (current-output-directory))
          (recursively-delete-directory (current-output-directory))))

      (del)
      (make-directory (current-output-directory))
      (define result
        (with-handlers ([exn?
                         (λ (e)
                           (del)
                           (raise e))])
          (f)))
      (del)
      result))

  (define-syntax-rule (with-test-musiclibrary body ...)
    (call/test-musiclibrary (λ () body ...)))

  ;; -----------------------------------

  (define test-audio-path
    (build-path (current-directory)
                "test-audio.ogg"))

  (check-pred file-exists? test-audio-path
              "example audio file (test-audio.ogg) must exist")

  (with-test-musiclibrary
    ; TODO: check metadata?? how??
    (process-track (track #:audio-src test-audio-path
                          #:output-path "test-audio.mp3"
                          'title "Foo"))
    (check-equal?
     (directory-list (current-output-directory))
     (list (build-path "test-audio.mp3")))))
