#lang racket/base
(provide
 (all-from-out "private/tracks-albums.rkt")
 generate-music-library)

(require
 "private/tracks-albums.rkt"
 "private/ffmpeg.rkt"
 racket/list
 racket/set
 racket/match
 racket/format)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; Utils
;; --------------------

;; (recursively-make-directory path) : void
;; path : path-string
(define (recursively-make-directory path)
  (let loop ([path (simplify-path path)])
    (define-values [root _final _must-be-dir?] (split-path path))
    (unless (directory-exists? path)
      (when (path? root)
        (loop root))
      (make-directory path))))

;; ---------------------------------------------------------------------------------------
;; Processing tracks
;; --------------------

;; (current-output-directory) : path
(define current-output-directory
  (make-parameter (build-path "./musiclibrary")))

;; (current-output-format) : (or 'mp3 'ogg)
(define current-output-format
  (make-parameter 'ogg))

;; -> bytes
(define (current-output-extension)
  (string->bytes/utf-8 (format ".~a" (current-output-format))))

;; (track-full-output-path trk) : path
;; trk : track
(define (track-full-output-path trk)
  (build-path (current-output-directory)
              (path-add-extension (track-output-path trk)
                                  (current-output-extension)
                                  #".")))

;; nat -> string
;; --
;; renders millisecond time value in FFmpeg-compatible "Time duration" form:
;; "<S>+[.<m>...]"
(define (format-ms ms)
  (~r (/ ms 1000)))

;; (track->ffmpeg-args trk) : [listof (or string path)]
;; trk : track
(define (track->ffmpeg-args trk)

  (define-values [src src-extra-flags]
    (let ([asrc (track-audio-source trk)])
      (cond
        [(audio-clip? asrc)
         (define ss (audio-clip-start/ms asrc))
         (define to (audio-clip-end/ms asrc))
         (values (audio-clip-source asrc)
                 (list* "-ss" (format-ms ss)
                        (if to (list "-to" (format-ms to)) '())))]
        [else
         (values asrc '())])))

  (define metadata-args
    (for/fold ([args
                ; -1 prevents metadata from input streams from automatically
                ; being copied over
                '("-map_metadata" "-1")])
              ([(k v) (in-track-metadata trk)])
      (define k-sym (metadata-key->symbol k (current-output-format)))
      (append args (list "-metadata:g" (format "~a=~a" k-sym v)))))

  `("-y"
    ,@src-extra-flags "-i" ,src
    ,@metadata-args
    ,(track-full-output-path trk)))

;; (process-track trk) : void
;; trk : track
;; --
;; raises exn:fail:ffmpeg
(define (process-track trk)
  (define-values [stdout stderr]
    (exec-ffmpeg (track->ffmpeg-args trk)))
  (void))

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
                "./example/test-audio.ogg"))

  (define test-track
    (track #:audio test-audio-path
           #:output "test-audio"
           (title: "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'mp3])
     (track->ffmpeg-args test-track))
   `("-y"
     "-i"
     ,test-audio-path
     "-map_metadata" "-1" "-metadata:g" "title=Foo"
     ,(build-path (current-output-directory) "test-audio.mp3")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track))
   `("-y"
     "-i"
     ,test-audio-path
     "-map_metadata" "-1" "-metadata:g" "TITLE=Foo"
     ,(build-path (current-output-directory) "test-audio.ogg")))

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
   `("-y"
     "-ss" "0.123"
     "-to" "100"
     "-i"
     ,test-audio-path
     "-map_metadata" "-1" "-metadata:g" "TITLE=Foo"
     ,(build-path (current-output-directory) "test-audio.ogg")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped*))
   `("-y"
     "-ss" "0.5"
     "-i"
     ,test-audio-path
     "-map_metadata" "-1" "-metadata:g" "TITLE=Foo"
     ,(build-path (current-output-directory) "test-audio.ogg")))


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
