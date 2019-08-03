#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; primitive sources
 source?
 (contract-out
  [fs (path-string? . -> . source?)]
  [net ((or/c string? url?) . -> . source?)])
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
 net/url-string
 "./time-value.rkt")

(module+ test
  (require rackunit))

(module* source-constructor-patterns #f
  (provide
   source:fs
   source:net))

;; ---------------------------------------------------------------------------------------
;; "Primitive" sources
;; -------------

;; --
;; TODO: some sort of "include path" for (fs ..) sources
;; TODO: multiple fs sources; something like (fs path-1 path-2 ...)
;; TODO: non-fs sources
;;  - yt-dl : yt-video-id -> source
;; --

(struct source []
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc src port write?)
     (if write?
       (write (source->sexp src) port)
       (write-string (source->pretty src) port)))])

;; path : path
(struct source:fs source [path] #:transparent)
;; url : url
(struct source:net source [url] #:transparent)

;; (fs p) : source
;; p : path-string
;; --
;; construct a source from a local file on the filesystem
(define (fs p)
  ; (simplify-path .. #f) means we don't hit the filesystem; this constructor shouldn't
  ; error or even do IO so it's important that #f is supplied.
  (source:fs (simplify-path (build-path p) #f)))

;; (net u) : source
;; u : (or url url-string)
;; --
;; construct a source from a remote file (by URL). the URL must be HTTP or HTTPS
(define (net u)
  (define l (if (url? u) u (string->url u)))
  (case (url-scheme l)
    [("http" "https") (void)]
    [else (raise-user-error 'net "URL must have schema \"http://\" or \"https://\"")])
  ;; TODO: is it valid to have non-empty 'url-fragment'?
  (source:net l))

;; (source->sexp src) : s-exp
;; src : source
(define (source->sexp src)
  (cond
    [(source:fs? src)
     `(fs ,(path->string (source:fs-path src)))]
    [(source:net? src)
     `(net ,(url->string (source:net-url src)))]))

;; (source->pretty src) : string
;; src : source
(define (source->pretty src)
  (cond
    [(source:fs? src)
     (format "file ~s" (path->string (source:fs-path src)))]
    [(source:net? src)
     (format "network URL ~s" (url->string (source:net-url src)))]))

(module+ test

  ;; this test shouldn't pass if we want to be able to search in multiple directories. but
  ;; then it makes these two paths considered unique
  ; (check-equal? (fs "../private/a") (fs "a"))

  (check-equal? (format "~a" (fs "foo")) "file \"foo\"")
  (check-equal? (format "~s" (fs "foo")) "(fs \"foo\")")
  (check-equal? (fs "foo/../a") (fs "a"))
  (check-equal? (net "http://a.com/?a=b;c=d") (net "http://a.com/?a=b&c=d")))

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
  (define s (source:fs (build-path "s")))
  (check-equal? (audio-clip* s '0:00 '1:00)
                (make-audio-clip s 0 60000))
  (check-equal? (audio-clip* s '2:01)
                (make-audio-clip s 121000 #f))
  (check-= (audio-clip-start (audio-clip* s '2:01.5))   121.5 0.0001)
  (check-= (audio-clip-end   (audio-clip* s 0 '2:01.5)) 121.5 0.0001)
  (check-false (audio-clip-end (audio-clip* s '2:01.5))))
