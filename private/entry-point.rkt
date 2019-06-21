#lang racket/base
(require racket/contract)

(provide
 (contract-out
  [generate-music-library ((listof album?) . -> . void?)]
  [cli-main ((listof album?) . -> . void?)]))

(require
 "./tracks-albums.rkt"
 "./process-track.rkt"
 "./ffmpeg.rkt"
 racket/cmdline
 racket/format
 racket/match)

(module+ test
  (require rackunit racket/port))

;; ---------------------------------------------------------------------------------------
;; Utils
;; --------------------

;; (plural n word [alt]) : string
;; n : nat
;; word, alt : string
(define (plural n word [alt (string-append word "s")])
  (if (= n 1) word alt))

;; ---------------------------------------------------------------------------------------
;; Fancy spinning indicator
;; --------------------

(define SPIN-CHARS "|/\u2014\\") ; \u2014 = "—" em dash

;; spin-index : nat
;; last-width : (or nat #f)
;; message : string
(struct indicator [(spin-index #:mutable)
                   (last-width #:mutable)
                   (message #:mutable)])

;; (make-indicator [msg]) : indicator
;; msg : string
(define (make-indicator [msg ""])
  (indicator 0 #f msg))

;; indicator -> void
(define (indicator-render! i)
  (define last-width (indicator-last-width i))
  (define msg (indicator-message i))
  (define pad-length (if last-width
                       (max 0 (- last-width (string-length msg)))
                       0))
  (display (~a (if last-width "\r" "")
               (string-ref SPIN-CHARS (indicator-spin-index i))
               " "
               msg
               (make-string pad-length #\space)))
  (flush-output)
  (set-indicator-last-width! i (string-length (indicator-message i))))

;; indicator -> void
(define (indicator-clear! i)
  (define last-width (indicator-last-width i))
  (when last-width
    (display (~a "\r"
                 (make-string (+ 2 (indicator-last-width i)) #\space)
                 "\r"))
    (flush-output)
    (set-indicator-last-width! i #f)))

;; indicator -> void
(define (indicator-spin! i)
  (define ix (indicator-spin-index i))
  (set-indicator-spin-index! i (remainder (add1 ix) (string-length SPIN-CHARS)))
  (indicator-render! i))

;; indicator string -> void
(define (indicator-update! i msg)
  (set-indicator-message! i msg)
  (indicator-render! i))

(module+ test
  (let ([i (make-indicator "hi")])
    (define-syntax-rule (check-ind (f arg ...) output)
      (check-equal? (with-output-to-string (λ () (f i arg ...))) output))
    (check-ind (indicator-render!)           "| hi")
    (check-ind (indicator-update! "world") "\r| world")
    (check-ind (indicator-update! "hi")    "\r| hi   ")
    (check-ind (indicator-spin!)           "\r/ hi")
    (check-ind (indicator-spin!)           "\r— hi")
    (check-ind (indicator-spin!)           "\r\\ hi")
    (check-ind (indicator-spin!)           "\r| hi")
    (check-ind (indicator-clear!)          "\r    \r")))

;; ---------------------------------------------------------------------------------------
;; Programmatic entry point
;; --------------------

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)

  (define tracks
    (for*/vector ([a (in-list library)]
                  [t (in-album-tracks a)])
      t))

  (define ind (make-indicator "Starting ..."))
  (define mailbox (make-channel))

  (define (ping-routine)
    (sleep 1/3)
    (channel-put mailbox 'ping)
    (ping-routine))

  (define (worker-routine)
    (recursively-make-directory (current-output-directory))
    (for ([trk (in-vector tracks)]
          [i (in-naturals)])
      (channel-put mailbox `(start ,i ,trk))
      (with-handlers ([exn:fail? (λ (e)
                                   (channel-put mailbox `(fail ,trk ,e)))])
        (process-track trk)
        (channel-put mailbox `(ok ,trk)))))

  (define ping-thread (thread ping-routine))
  (define worker-thread (thread worker-routine))

  (printf "* Processing ~a ~a\n"
          (vector-length tracks)
          (plural (vector-length tracks) "track"))

  (let loop ()
    (define v (sync mailbox worker-thread))
    (unless (eq? v worker-thread)
      (match v
        ['ping
         (indicator-spin! ind)]
        [`(start ,i ,trk)
         (indicator-update! ind (format "(~a/~a) Processing track: ~s"
                                        (add1 i)
                                        (vector-length tracks)
                                        (track-title trk)))]
        [`(fail ,trk ,e)
         (indicator-clear! ind)
         (printf "* Failed: ~s\n\n~a\n\n"
                 (track-title trk)
                 (exn-message e))]
        [`(ok ,trk)
         (void)])
      (loop)))

  (break-thread ping-thread)
  (printf "\n* Completed\n"))

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
;; CLI entry point
;; --------------------

;; (cli-main library [#:argv argv]) : void
;; library : (listof album)
;; argv : (listof string)
(define (cli-main library
                  #:argv [argv (current-command-line-arguments)])
  (command-line
   #:argv argv
   #:once-each

   (("-o" "--output-dir")
    out-dir
    [(format "Output directory (default: '~a')" (current-output-directory))]
    (current-output-directory (build-path out-dir)))

   (("-f" "--format")
    fmt
    [(format "File format; either mp3 or ogg (default: ~a)" (current-output-format))]
    (current-output-format
     (case fmt
       [("mp3") 'mp3]
       [("ogg") 'ogg]
       [else (error "invalid output format ~s" fmt)])))

   (("--ffmpeg")
    path
    [(format "Path to FFmpeg executable (default: '~a')" (current-ffmpeg))]
    (current-ffmpeg (build-path path))))

  ;; -------

  (generate-music-library library))
