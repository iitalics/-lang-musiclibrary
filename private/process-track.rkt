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
  [process-track (track? #:cache source-cache? . -> . void?)]))

(require
 "./tracks-albums.rkt"
 "./ffmpeg.rkt"
 "./metadata.rkt"
 "./source.rkt"
 "./source/cache.rkt"
 "./source/fetch.rkt"
 racket/format
 racket/match
 threading)

(module+ test
  (require
   rackunit
   racket/set
   "./test-utils.rkt"
   (submod "./source/cache.rkt" test-utils)))

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

;; (process-track trk #:cache cache) : void
;; trk : track
;; cache : source-cache
;; --
;; update-cache should return a cache which contains the given source.
;; raises exn:fail:ffmpeg if process fails.
;; raises exn:fail:source-cache-miss if any required sources were not found
(define (process-track trk #:cache cache)
  (define-values [_stdout _stderr]
    (exec-ffmpeg (track->ffmpeg-args trk #:cache cache)))
  (void))

;; (track->ffmpeg-args trk #:cache sc) : ffmpeg-args
;; trk : track
;; sc : source-paths
;; --
;; raises exn:fail:source-cache-miss if any required sources were not found
(define (track->ffmpeg-args trk #:cache sc)
  (define-values [a-src a-flags]
    (match (track-audio-source trk)
      [(? audio-clip? c)
       (define ss (audio-clip-start/ms c))
       (define to (audio-clip-end/ms c))
       (values (audio-clip-source c)
               (list* "-ss"
                      (~r (/ ss 1000))
                      (if to
                        `("-to" ,(~r (/ to 1000)))
                        '())))]
      [(? source? src)
       (values src '())]))

  (define a-path
    (source-cache-ref sc a-src))

  (for/fold ([args (~> (track-full-output-path trk)
                       make-ffmpeg-args
                       (ffmpeg-args-add-input _ a-path a-flags))])
            ([m-e (in-track-metadata trk)])
    (apply-metadata-entry m-e
                          args
                          #:format (current-output-format)
                          #:cache sc)))

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

  (define test-audio-path (build-path (current-directory) "../example/test-audio.ogg"))
  (define test-image-path (build-path (current-directory) "../example/lain.png"))
  (define test-audio-src (fs test-audio-path))
  (define test-image-src (fs test-image-path))

  (define test-track
    (track #:audio test-audio-src
           #:output "test-audio"
           (title: "Foo")))

  (define cache-full
    (hash test-audio-src test-audio-path
          test-image-src test-image-path))

  ;; --
  ;; ffmpeg-args for tracks with simple metadata & source

  (check-exn (cache-miss= test-audio-src)
             (λ ()
               (parameterize ([current-output-format 'mp3])
                 (track->ffmpeg-args test-track #:cache (hash)))))

  (check-equal?
   (parameterize ([current-output-format 'mp3])
     (track->ffmpeg-args test-track #:cache cache-full))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.mp3"))
       (ffmpeg-args-add-input test-audio-path '())
       (ffmpeg-args-set-metadata _ 'title "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track #:cache cache-full))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '())
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

  ;; --
  ;; ffmpeg-args for tracks with complex metadata (album cover, track number)

  ;; PROBLEM: we can't test adding multiple simple metadata because the hash key order is
  ;; random...

  (define test-track+cover
    (track #:audio test-audio-src
           #:output "test-audio"
           (cover-art: test-image-src)
           (track-num: 8)))

  (check-equal?
   (parameterize ([current-output-format 'mp3])
     (track->ffmpeg-args test-track+cover #:cache cache-full))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.mp3"))
       (ffmpeg-args-add-input test-audio-path '())
       (ffmpeg-args-add-input test-image-path '())
       (ffmpeg-args-set-metadata _ 'track "8")))

  (check-exn (cache-miss= test-image-src)
             (λ ()
               (parameterize ([current-output-format 'mp3])
                 (track->ffmpeg-args test-track+cover
                                     #:cache
                                     (hash test-audio-src test-audio-path)))))

  ;; --
  ;; ffmpeg-args for tracks with clipped sources

  (define test-track-clipped
    (track #:audio (audio-clip test-audio-src 0.123 '1:40)
           #:output "test-audio"
           (title: "Foo")))

  (define test-track-clipped*
    (track #:audio (audio-clip test-audio-src 0.5)
           #:output "test-audio"
           (title: "Foo")))

  (check-exn (cache-miss= test-audio-src)
             (λ ()
               (parameterize ([current-output-format 'ogg])
                 (track->ffmpeg-args test-track-clipped #:cache (hash)))))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped #:cache cache-full))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '("-ss" "0.123" "-to" "100"))
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

  (check-equal?
   (parameterize ([current-output-format 'ogg])
     (track->ffmpeg-args test-track-clipped* #:cache cache-full))
   (~> (make-ffmpeg-args (build-path (current-output-directory) "test-audio.ogg"))
       (ffmpeg-args-add-input test-audio-path '("-ss" "0.5"))
       (ffmpeg-args-set-metadata _ 'TITLE "Foo")))

  ;; -----------------------------------
  ;; IO performing tests

  (check-pred file-exists? test-audio-path
              "example audio file (test-audio.ogg) must exist")
  (check-pred file-exists? test-image-path
              "example image file (lain.png) must exist")

  (with-test-musiclibrary
    (parameterize ([current-output-format 'mp3])
      (process-track test-track+cover #:cache cache-full))

    ; test it generated a file
    (check-equal? (directory-list (current-output-directory))
                  (list (build-path "test-audio.mp3")))))
