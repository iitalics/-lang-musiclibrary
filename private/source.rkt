#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; primitive sources
 source? source-cache?
 (contract-out
  (struct (exn:fail:fetch exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [reason exn?]))
  ; *** constructors ***
  [fs (path-string? . -> . source?)]
  ; *** operations on sources ***
  [source-fetch (source? . -> . path?)])
 ; ---
 ; audio sources
 audio-source? audio-clip?
 (contract-out
  [rename audio-clip* audio-clip
          ((source? time-value?) ; src start
           ((or/c time-value? #f)) ; end
           . ->* . audio-clip?)]
  [make-audio-clip (source? ; src
                    exact-nonnegative-integer? ; start
                    (or/c exact-nonnegative-integer? #f) ;end
                    . -> . audio-clip?)]
  [audio-clip-source   (audio-clip? . -> . source?)]
  [audio-clip-start/ms (audio-clip? . -> . exact-nonnegative-integer?)]
  [audio-clip-start    (audio-clip? . -> . flonum?)]
  [audio-clip-end/ms   (audio-clip? . -> . (or/c exact-nonnegative-integer? #f))]
  [audio-clip-end      (audio-clip? . -> . (or/c flonum? #f))]))

(require
 racket/path
 "./time-value.rkt"
 (only-in racket/function curry))

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; "Primitive" sources
;; -------------

;; --
;; TODO: some sort of "include path" for (fs ..) sources
;; TODO: multiple fs sources; something like (fs path-1 path-2 ...)
;; TODO: non-fs sources
;;  - url  : url -> source
;;  - yt-dl : yt-video-id -> source
;; --

(struct source []
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc src port mode)
     (write (source->sexp src) port))])

;; path : path
(struct source:fs source [path] #:transparent)

;; (fs p) : source
;; p : path-string
;; --
;; construct a source from a local file on the filesystem
(define (fs p)
  ; (simplify-path .. #f) means we don't hit the filesystem; this constructor shouldn't
  ; error or even do IO so it's important that #f is supplied.
  (source:fs (simplify-path (build-path p) #f)))

;; (source->sexp src) : s-exp
;; src : source
(define (source->sexp src)
  (cond
    [(source:fs? src)
     `(fs ,(path->string (source:fs-path src)))]))

(module+ test

  ;; this test shouldn't pass if we want to be able to search in multiple directories. but
  ;; then it makes these two paths considered unique
  ;; (check-equal? (fs "../private/a") (fs "a"))

  (check-equal? (format "~s" (fs "foo")) "(fs \"foo\")")
  (check-equal? (fs "foo/../a") (fs "a")))

;; ---------------------------------------------------------------------------------------
;; Fetching sources
;; --------

; TODO:
; ;; (current-cache-directory) : path
; (define current-cache-directory
;   (make-parameter (build-path "./musiclibrary-cache")))

;; (make-exn:fail:fetch msg cmarks reason)
;; reason : exn
(struct exn:fail:fetch exn:fail [reason]
  #:extra-constructor-name make-exn:fail:fetch)

;; source exn -> !
(define (reraise-fetch-exn src err)
  (define msg (format "failed to fetch source: ~s\n  reason: ~a" src (exn-message err)))
  (raise (make-exn:fail:fetch msg (current-continuation-marks) err)))

;; (source-fetch src) : path
;; src : source
;; ---
;; raises 'exn:fail:fetch' on error
(define (source-fetch src)
  (with-handlers ([exn:fail:fetch? raise]
                  [exn:fail? (curry reraise-fetch-exn src)])
    (cond
      [(source:fs? src)
       ; TODO: search multiple directories for the absolute path?
       (define path (normalize-path (source:fs-path src)))
       (unless (file-exists? path)
         (define msg (format "source file ~s not found" (path->string path)))
         (raise (make-exn:fail:filesystem msg (current-continuation-marks))))
       path])))

;; source-cache ::= [hash source => path]
(define (source-cache? h)
  (and (hash? h)
       (for/and ([(k v) (in-hash h)])
         (and (source? k) (path? v)))))

;; ==========================================

(module+ test
  (check-equal? (source-fetch (fs "../example/lain.png"))
                (normalize-path (build-path "../example/lain.png")))

  (for ([bad-path (in-list '("../example/doesnt-exist.png"
                             "../example"))])
    (check-exn (λ (e) (and (exn:fail:fetch? e)
                           (exn:fail:filesystem? (exn:fail:fetch-reason e))
                           (regexp-match? #px"reason: source file .* not found"
                                          (exn-message e))))
               (λ () (source-fetch (fs bad-path))))))

;; ---------------------------------------------------------------------------------------
;; Audio sources (i.e., possibly-clipped sources)
;; -------------

;; audio-source ::= source | audio-clip
(define (audio-source? x)
  (or (source? x)
      (audio-clip? x)))

;; source : source
;; start/ms : nat
;; end/ms : (or nat #f)
(struct audio-clip [source start/ms end/ms]
  #:transparent
  #:extra-constructor-name make-audio-clip)

;; (audio-clip* src start [end]) : audio-clip
;; src : source
;; start : time-value
;; end : (or time-value #f)
(define (audio-clip** src start [end #f])
  (make-audio-clip src
                   (time-value->milliseconds start)
                   (and end (time-value->milliseconds end))))
(define audio-clip*
  (procedure-rename audio-clip** 'audio-clip))

;; audio-clip -> float
(define (audio-clip-start ac)
  (ms->s (audio-clip-start/ms ac)))

;; audio-clip -> (or float #f)
(define (audio-clip-end ac)
  (cond [(audio-clip-end/ms ac) => ms->s]
        [else #f]))

(define (ms->s x)
  (* x 0.001))

(module+ test
  (define s (build-path "s"))
  (check-equal? (audio-clip* s '0:00 '1:00)
                (make-audio-clip s 0 60000))
  (check-equal? (audio-clip* s '2:01)
                (make-audio-clip s 121000 #f))
  (check-= (audio-clip-start (audio-clip* s '2:01.5))   121.5 0.0001)
  (check-= (audio-clip-end   (audio-clip* s 0 '2:01.5)) 121.5 0.0001)
  (check-false (audio-clip-end (audio-clip* s '2:01.5))))
