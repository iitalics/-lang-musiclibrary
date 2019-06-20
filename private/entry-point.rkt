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
 racket/format)

;; ---------------------------------------------------------------------------------------
;; Programmatic entry point
;; --------------------

;; (generate-music-library library) : void
;; library : (listof album)
(define (generate-music-library library)
  (recursively-make-directory (current-output-directory))
  (for* ([a (in-list library)]
         [t (in-album-tracks a)])
    (printf "Processing: ~a\n" t)
    (process-track t)))

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
