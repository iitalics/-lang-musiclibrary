#lang racket/base
(require
 "private/tracks-albums.rkt"
 "private/ffmpeg.rkt"
 racket/list)

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

;; (track->ffmpeg-args trk) : [listof (or string path)]
;; trk : track
(define (track->ffmpeg-args trk)
  (define metadata-args
    (append* (for/list ([(k v) (in-hash (track-meta trk))])
               (list "-metadata:g" (format "~a=~a" k v)))))
  `("-y"
    "-i"
    ,(track-audio-src trk)
    ,@metadata-args
    ,(build-path (current-output-directory)
                 (track-output-path trk))))

;; (process-track trk) : void
;; trk : track
;; --
;; raises exn:fail:ffmpeg
(define (process-track trk)
  (define-values [stdout stderr]
    (exec-ffmpeg (track->ffmpeg-args trk)))
  (void))

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
  ;; Simple unit tests

  (define test-audio-path
    (build-path (current-directory)
                "test-audio.ogg"))

  (define test-track
    (track #:audio-src test-audio-path
           #:output-path "test-audio.mp3"
           'title "Foo"))

  (check-equal?
   (track->ffmpeg-args test-track)
   `("-y"
     "-i"
     ,test-audio-path
     "-metadata:g" "title=Foo"
     ,(build-path (current-output-directory) "test-audio.mp3")))

  ;; -----------------------------------
  ;; IO performing tests

  (check-pred file-exists? test-audio-path
              "example audio file (test-audio.ogg) must exist")

  (with-test-musiclibrary
    ; TODO: check metadata?? how??
    (process-track test-track)
    (check-equal?
     (directory-list (current-output-directory))
     (list (build-path "test-audio.mp3")))))
