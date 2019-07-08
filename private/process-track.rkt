#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; config
 valid-output-format?
 (contract-out
  [current-output-directory (parameter/c path?)]
  [current-output-format (parameter/c valid-output-format?)]
  [output-extension (case-> (-> bytes?) (valid-output-format? . -> . bytes?))])
 ; ---
 ; process-track
 (contract-out
  [track-cached? (track? . -> . boolean?)]
  [process-track (track? . -> . void?)]))

(require
 "./tracks-albums.rkt"
 "./ffmpeg.rkt"
 "./metadata.rkt"
 racket/format)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Configuration
;; --------------------

(define (valid-output-format? x)
  (or (eq? x 'mp3)
      (eq? x 'ogg)))

;; (current-output-directory) : path
(define current-output-directory
  (make-parameter (build-path "./musiclibrary")))

;; (current-output-format) : valid-output-format
(define current-output-format
  (make-parameter 'ogg))

;; (output-extension [fmt]) -> bytes
;; fmt : valid-output-format
(define (output-extension [fmt (current-output-format)])
  (string->bytes/utf-8 (format ".~a" fmt)))

;; ---------------------------------------------------------------------------------------
;; Processing tracks
;; --------------------

;; (track-cached? trk) : boolean
;; trk : track
(define (track-cached? trk)
  (file-exists? (track-full-output-path trk)))

;; (process-track trk) : void
;; trk : track
;; --
;; raises exn:fail:ffmpeg
(define (process-track trk)
  (define-values [stdout stderr]
    (exec-ffmpeg (track->ffmpeg-args trk)))
  (void))

;; (track->ffmpeg-args trk) : ffmpeg-args
;; trk : track
(define (track->ffmpeg-args trk)

  (define-values [src src-flags]
    (let ([asrc (track-audio-source trk)])
      (cond
        [(audio-clip? asrc)
         (define ss (audio-clip-start/ms asrc))
         (define to (audio-clip-end/ms asrc))
         (values (audio-clip-source asrc)
                 (list* "-ss" (~r (/ ss 1000))
                        (if to (list "-to" (~r (/ to 1000))) '())))]
        [(source? asrc)
         (values asrc '())])))

  (let* ([args
          (make-ffmpeg-args (track-full-output-path trk)
                            #:asrc-path src
                            #:asrc-flags src-flags)]
         [args ; add metadata
          (for/fold ([args args])
                    ([(k v) (in-track-metadata trk)])
            (ffmpeg-args-set-metadata args
                                      (metadata-key->symbol k (current-output-format))
                                      v))])
    args))

;; (track-full-output-path trk) : path
;; trk : track
(define (track-full-output-path trk)
  (build-path (current-output-directory)
              (path-add-extension (track-output-path trk)
                                  (output-extension)
                                  #".")))

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
    (parameterize ([current-output-directory (build-path "./test-musiclibrary")])
      (define (del)
        (when (directory-exists? (current-output-directory))
          (recursively-delete-directory (current-output-directory))))

      (make-directory (current-output-directory))
      (define result
        (with-handlers ([exn? (λ (e) (del) (raise e))])
          (f)))
      (del)
      result))

  (define-syntax-rule (with-test-musiclibrary body ...)
    (call/test-musiclibrary (λ () body ...)))

  ;; -----------------------------------
  ;; Simple unit tests

  (define test-audio-path
    (build-path (current-directory)
                "../example/test-audio.ogg"))

  (define test-track
    (track #:audio test-audio-path
           #:output "test-audio"
           (title: "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'mp3])
     (track->ffmpeg-args test-track))
   (ffmpeg-args-set-metadata
    (make-ffmpeg-args (build-path (current-output-directory) "test-audio.mp3")
                      #:asrc-path test-audio-path
                      #:asrc-flags '())
    'title
    "Foo"))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track))
   (ffmpeg-args-set-metadata
    (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg")
                      #:asrc-path test-audio-path
                      #:asrc-flags '())
    'TITLE
    "Foo"))

  (define test-track-clipped
    (track #:audio (audio-clip test-audio-path 0.123 '1:40)
           #:output "test-audio"
           (title: "Foo")))

  (define test-track-clipped*
    (track #:audio (audio-clip test-audio-path 0.5)
           #:output "test-audio"
           (title: "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped))
   (ffmpeg-args-set-metadata
    (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg")
                      #:asrc-path test-audio-path
                      #:asrc-flags '("-ss" "0.123" "-to" "100"))
    'TITLE
    "Foo"))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped*))
   (ffmpeg-args-set-metadata
    (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg")
                      #:asrc-path test-audio-path
                      #:asrc-flags '("-ss" "0.5"))
    'TITLE
    "Foo"))

  ;; -----------------------------------
  ;; IO performing tests

  (check-pred file-exists? test-audio-path
              "example audio file (test-audio.ogg) must exist")

  (with-test-musiclibrary
    ; TODO: check metadata?? how??
    (parameterize ([current-output-format 'mp3])
      (process-track test-track))
    (check-equal?
     (directory-list (current-output-directory))
     (list (build-path "test-audio.mp3")))))
