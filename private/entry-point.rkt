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

;; (recursively-make-directory path) : void
;; path : path-string
(define (recursively-make-directory path)
  (let loop ([path (simplify-path path)])
    (define-values [root _final _must-be-dir?] (split-path path))
    (unless (directory-exists? path)
      (when (path? root)
        (loop root))
      (make-directory path))))

(define-syntax-rule (loop-forever body ...)
  (let loop ()
    (let () body ...)
    (loop)))

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

;; (current-number-of-jobs) : positive-nat
(define current-number-of-jobs
  (make-parameter 8))

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)

  (define mailbox
    (make-channel))

  (define-values [tracks n-cached]
    (for*/fold ([tracks '()] [n-cached 0])
               ([a (in-list library)]
                [t (in-album-tracks a)])
      (if (track-cached? t)
        (values tracks (add1 n-cached))
        (values (cons t tracks) n-cached))))

  (define n-tracks
    (length tracks))

  ;; ----
  ;; worker

  (define (worker-routine)
    (loop-forever
     (define trk
       (let ([getter (make-channel)])
         (channel-put mailbox `(wait ,getter))
         (channel-get getter)))

     (with-handlers ([exn:fail? (λ (e)
                                  (channel-put mailbox `(fail ,trk ,e)))])
       (process-track trk)
       (channel-put mailbox `(ok ,trk)))))

  ;; ----
  ;; ping

  (define (ping-routine)
    (loop-forever
     (sleep 1/3)
     (channel-put mailbox 'ping)))

  ;; ----
  ;; mailbox loop

  (define (message n-complete . stuff)
    (format "(~a/~a) ~a"
            n-complete
            n-tracks
            (apply ~a stuff)))

  (define ind
    (make-indicator (message 0 "Started.")))

  ;; tq : (listof track)
  ;; n-complete : nat
  (define (mail-loop tq n-complete)
    (unless (>= n-complete
                n-tracks)
      (match (channel-get mailbox)
        ['ping
         (indicator-spin! ind)
         (mail-loop tq n-complete)]

        [`(ok ,trk)
         (define n-complete* (add1 n-complete))
         (indicator-update! ind (message n-complete*
                                         "Finished: "
                                         (~s (track-title trk))))
         (mail-loop tq n-complete*)]

        [`(fail ,trk ,e)
         (define n-complete* (add1 n-complete))
         (indicator-clear! ind)
         (displayln (exn-message e))
         (indicator-update! ind (message n-complete*
                                         "Failed: "
                                         (~s (track-title trk))))
         (mail-loop tq n-complete*)]

        [`(wait ,getter)
         (define tq* (cond
                       [(null? tq) '()]
                       [else (channel-put getter (car tq))
                             (cdr tq)]))
         (mail-loop tq* n-complete)])))

  ;; ---
  ;; starts here

  (printf "* Processing ~a ~a\n"
          n-tracks
          (plural n-tracks "track"))

  (unless (zero? n-cached)
    (printf "* (skipping ~a ~a)\n"
            n-cached
            (plural n-cached "track")))

  (recursively-make-directory (current-output-directory))

  (define worker-threads
    (for/list ([i (in-range (current-number-of-jobs))])
      (thread worker-routine)))

  (define ping-thread
    (thread ping-routine))

  (mail-loop tracks 0)

  (for ([thd (in-list (cons ping-thread worker-threads))])
    (break-thread thd))

  (printf "\n* Completed\n"))

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
