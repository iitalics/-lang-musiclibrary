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
 "./source.rkt"
 "./source/cache.rkt"
 "./source/fetch.rkt"
 "./utils.rkt"
 racket/cmdline
 racket/format
 racket/match
 racket/set
 racket/string
 threading)

(module+ test
  (require
   rackunit
   racket/port
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

;; (current-skip-tracks?) : boolean
(define current-skip-tracks?
  (make-parameter #t))

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)

  (define-values [all-tracks n-skipped]
    (for*/fold ([tracks (set)] [n-skipped 0])
               ([a (in-list library)]
                [t (in-album-tracks a)])
      (if (and (current-skip-tracks?)
               (track-already-exists? t))
        (values tracks (add1 n-skipped))
        (values (set-add tracks t) n-skipped))))

  (unless (zero? n-skipped)
    (printf "* Skipping ~a ~a.\n" n-skipped (plural n-skipped "track")))

  ;; task = source | track
  ;; result = `(ok any) | `(failed exn) | `(blocked source)

  ;; (work t sc) : result
  ;; t : task
  ;; sc : source-cache
  (define (work t sc)
    (with-handlers ([exn:fail:source-cache-miss?
                     (λ (e)
                       `(blocked ,(exn:fail:source-cache-miss-source e)))]
                    [exn?
                     (λ (e)
                       `(failed ,e))])
      `(ok ,(match t
              [(? source? src) (source-fetch src)]
              [(? track? trk) (process-track trk #:cache sc)]))))

  ;; tq : [listof task]
  ;; sc : source-cache
  ;; dep : [hash source => [listof task]]
  ;; pending : [set task]
  (define (main-loop tq sc dep pending)
    (cond
      [(pair? tq)
       (define task (car tq))
       ; TODO: queue of "free worker threads"
       (thread (λ ()
                 (~> (work task sc)
                     (append _ (list task))
                     (thread-send main-thread _))))
       (thread-send ui-thread `(started ,task))
       (main-loop (cdr tq)
                  sc
                  dep
                  (set-add pending task))]

      [(set-empty? pending)
       (thread-send ui-thread 'goodbye)]

      ['()
       (match (thread-receive)
         [`(ok ,_ ,(? track? trk))
          (thread-send ui-thread `(finished ,trk))
          (main-loop tq
                     sc
                     dep
                     (set-remove pending trk))]

         [`(ok ,path ,(? source? src))
          (main-loop (append (hash-ref dep src '()) tq)
                     (source-cache-add sc src path)
                     dep
                     (set-remove pending src))]

         [`(blocked ,src ,task)
          (cond
            [(source-in-cache? sc src)
             (main-loop (cons task tq) sc dep pending)]
            [else
             (main-loop (if (set-member? pending src) tq (cons src tq))
                        sc
                        (hash-update dep src (λ (ts) (cons task ts)) '())
                        (set-remove (set-add pending src) task))])]

         [`(failed ,e ,task)
          (thread-send ui-thread `(error ,e))
          (main-loop tq
                     sc
                     dep
                     (set-remove pending task))])]))

  (define (ui-loop ind n errs)
    (define (msg . args)
      (string-append (format "(~a/~a) " n (set-count all-tracks))
                     (apply format args)))
    (match (thread-receive)
      ['ping
       (thread (λ ()
                 (sleep 1/3)
                 (with-handlers ([exn:fail:contract? void])
                   (thread-send ui-thread 'ping))))
       (indicator-spin! ind)
       (ui-loop ind n errs)]

      [`(started ,(? track? trk))
       (ui-loop ind n errs)]

      [`(started ,(? source? src))
       (indicator-update! ind (msg "Fetching ~a" src))
       (ui-loop ind n errs)]

      [`(finished ,trk)
       (indicator-update! ind (msg "Finished: ~s" (track-title trk)))
       (ui-loop ind (add1 n) errs)]

      [`(error ,e)
       (define 1st-line (car (string-split (exn-message e) "\n")))
       (indicator-update! ind (msg "Error: ~a" 1st-line))
       (ui-loop ind n (cons e errs))]

      ['goodbye
       (indicator-update! ind (msg "Finished."))
       (unless (null? errs)
         (printf "\n=====\nSome errors occured while processing:\n")
         (for ([e (in-list errs)])
           (printf "~a\n"
                   (prepend-lines "> " (exn-message e)))))]))

  (recursively-make-directory (current-output-directory))

  (define ui-thread
    (thread (λ () (ui-loop (make-indicator "Started")
                           0
                           '()))))

  (define main-thread
    (thread (λ () (main-loop (set->list all-tracks)
                             empty-source-cache
                             (hash)
                             (set)))))

  (void (thread-send ui-thread 'ping)
        (sync main-thread)
        (sync ui-thread)))

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
    (current-skip-tracks? #f))

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
