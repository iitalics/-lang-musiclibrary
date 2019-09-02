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
 "./source/cache.rkt"
 "./utils.rkt"
 racket/cmdline
 racket/format
 racket/match
 racket/string
 threading)

(module+ test
  (require
   rackunit
   racket/port
   "./source.rkt"
   "./metadata.rkt"
   "./test-utils.rkt"))

;; ---------------------------------------------------------------------------------------
;; Utils
;; --------------------

;; (prepend-to-lines prefix str #:skip-first? [skip-first?]) : string
;; prefix, str : string
;; skip-first? : boolean
(define (prepend-lines prefix str #:skip-first? [sk? #f])
  (define (prepend-lines/list lines)
    (for/list ([i (in-naturals)]
               [line (in-list lines)])
      (if (and sk? (zero? i))
        line
        (string-append prefix line))))

  (~> str
      (string-split _ "\n")
      prepend-lines/list
      (string-join _ "\n")))

(define-syntax-rule (loop-forever body ...)
  (let loop ()
    (let () body ...)
    (loop)))

(module+ test
  (check-equal? (prepend-lines "* " "a\nb\nc")
                "* a\n* b\n* c")
  (check-equal? (prepend-lines "* " "a\nb\nc" #:skip-first? #t)
                "a\n* b\n* c"))

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

;; (current-skip-cached-tracks?) : boolean
(define current-skip-cached-tracks?
  (make-parameter #t))

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)

  (define mailbox
    (make-channel))

  (define-values [tracks n-cached]
    (for*/fold ([tracks '()] [n-cached 0])
               ([a (in-list library)]
                [t (in-album-tracks a)])
      (if (and (current-skip-cached-tracks?)
               (track-already-exists? t))
        (values tracks (add1 n-cached))
        (values (cons t tracks) n-cached))))

  (define n-tracks
    (length tracks))

  ;; ----
  ;; worker

  (define (worker-routine)
    (define recv (make-channel))
    (define (update-cache src)
      (channel-put mailbox `(fetch ,src ,recv))
      (channel-get recv))
    (loop-forever
     (channel-put mailbox `(wait ,recv))
     (define trk (channel-get recv))
     (with-handlers ([exn:fail? (λ (e)
                                  (channel-put mailbox `(fail ,trk ,e)))])
       (process-track trk update-cache)
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
  ;; sc : source-cache
  ;; nc : nat
  (define (mail-loop tq sc nc)
    (unless (>= nc n-tracks)
      (match (channel-get mailbox)
        ['ping
         (indicator-spin! ind)
         (mail-loop tq sc nc)]

        [`(ok ,trk)
         (define nc* (add1 nc))
         (indicator-update! ind (message nc*
                                         "Finished: "
                                         (~s (track-title trk))))
         (mail-loop tq sc nc*)]

        [`(fail ,trk ,e)
         (define nc* (add1 nc))
         (indicator-clear! ind)
         (displayln (exn-message e))
         (indicator-update! ind (message nc*
                                         "Failed: "
                                         (~s (track-title trk))))
         (mail-loop tq sc nc*)]

        [`(wait ,recv-chan)
         (define tq* (cond
                       [(null? tq) '()]
                       [else (channel-put recv-chan (car tq))
                             (cdr tq)]))
         (mail-loop tq* sc nc)]

        [`(fetch ,src ,recv-chan)
         (define sc*
           (if (source-in-cache? sc src)
             sc
             (begin0
                 (source-cache sc src)
               (indicator-update! ind (message nc "Fetched: " (~a src))))))
         (channel-put recv-chan sc*)
         (mail-loop tq sc* nc)])))

  (define (start-mail-loop)
    (mail-loop tracks (hash) 0))

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

  (define result
    (with-handlers ([exn:fail? (λ (e) e)])
      (start-mail-loop)
      'ok))

  (for ([thd (in-list (cons ping-thread worker-threads))])
    (break-thread thd))

  (match result
    ['ok
     (printf "\n* Completed\n")]
    [(? exn? e)
     (printf "\n* Failed: ")
     (displayln (prepend-lines "* "
                               (exn-message e)
                               #:skip-first? #t))]))


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
       [else (error (format "invalid output format: ~s" fmt))])))

   (("-j" "--jobs")
    num
    [(format "Number of jobs (default: ~a)" (current-number-of-jobs))]
    (define num* (string->number num))
    (if (and (exact-integer? num*)
             (>= num* 1))
      (current-number-of-jobs num*)
      (error (format "invalid number of jobs: ~s" num))))

   (("--force")
    "Don't skip generating tracks if the file already exists"
    (current-skip-cached-tracks? #f))

   (("--ffmpeg")
    path
    [(format "Path to FFmpeg executable (default: '~a')" (current-ffmpeg))]
    (current-ffmpeg (build-path path))))

  ;; -------

  (generate-music-library library))

(module+ test

  ;; these tests do IO

  (define (test-main)
    (cli-main
     (list (album "The Test"
                  (cover-art: (fs "../example/lain.png"))
                  (track #:audio (fs "../example/test-audio.ogg") (title: "Test1"))
                  (track #:audio (fs "../example/test-audio.ogg") (title: "Test2"))))
     #:argv
     '("-o" "TEST-MUSICLIB" "-j" "2" "-f" "mp3")))

  (void
   (with-handlers ([exn:fail? (λ (e)
                                (check-false e))])
     (with-output-to-string test-main)
     ; run again so it is cached this time
     (with-output-to-string test-main)))

  (recursively-delete-directory "./TEST-MUSICLIB"))
