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
  [track-already-exists? (track? . -> . boolean?)]
  [process-track (track? . -> . void?)]))

(require
 "./tracks-albums.rkt"
 "./ffmpeg.rkt"
 "./metadata.rkt"
 "./source.rkt"
 racket/format
 threading)

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

;; (track-already-exists? trk) : boolean
;; trk : track
(define (track-already-exists? trk)
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

  (define (add-audio-src args)
    (define asrc (track-audio-source trk))
    (cond
      [(audio-clip? asrc)
       (define ss (audio-clip-start/ms asrc))
       (define to (audio-clip-end/ms asrc))
       (define src-path (source-fetch (audio-clip-source asrc)))
       (ffmpeg-args-add-input args
                              src-path
                              (list* "-ss"
                                     (~r (/ ss 1000))
                                     (if to
                                       `("-to" ,(~r (/ to 1000)))
                                       '())))]

      [(source? asrc)
       (define src-path (source-fetch asrc))
       (ffmpeg-args-add-input args
                              src-path
                              '())]))

  (define (add-metadata args)
    (for/fold ([args args])
              ([m-e (in-track-metadata trk)])
      (apply-metadata-entry m-e
                            args
                            #:format (current-output-format))))

  (~> (track-full-output-path trk)
      make-ffmpeg-args
      add-audio-src
      add-metadata))

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
    (track #:audio (fs test-audio-path)
           #:output "test-audio"
           (title: "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'mp3])
     (track->ffmpeg-args test-track))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.mp3"))
       (ffmpeg-args-add-input test-audio-path '())
       (ffmpeg-args-set-metadata _ 'title "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '())
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

  (define test-track-clipped
    (track #:audio (audio-clip (fs test-audio-path) 0.123 '1:40)
           #:output "test-audio"
           (title: "Foo")))

  (define test-track-clipped*
    (track #:audio (audio-clip (fs test-audio-path) 0.5)
           #:output "test-audio"
           (title: "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '("-ss" "0.123" "-to" "100"))
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped*))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '("-ss" "0.5"))
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

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
