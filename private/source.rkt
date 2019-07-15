#lang racket/base
(require racket/contract)

(provide
 ; ---
 ; primitive sources
 source?
 (contract-out
  ; *** constructors ***
  [fs (path-string? . -> . source?)]
  ; *** operations on sources ***
  [source-path (source? . -> . path?)])
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
 "./time-value.rkt")

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------
;; "Primitive" sources
;; -------------

;; --
;; TODO: some sort of "include path" for (fs ..) sources
;; TODO: check if source is valid
;; TODO: non-fs sources
;;  - url  : url -> source
;;  - yt-dl : yt-video-id -> source
;; --

(struct source []
  #:methods gen:custom-write
  [(define (write-proc src port mode)
     (write (source->sexp src) port))])

;; path : path
(struct source:fs source [path])

;; (fs p) : source
;; p : path-string
;; --
;; construct a source from a local file on the filesystem
(define (fs p)
  (source:fs (build-path p)))

;; (source->sexp src) : s-exp
;; src : source
(define (source->sexp src)
  (cond
    [(source:fs? src) `(fs ,(path->string (source:fs-path src)))]))

;; (source-path src) : path
;; src : source
(define (source-path src)
  (cond
    [(source:fs? src) (source:fs-path src)]))

;; ==========================================

(module+ test
  (check-equal? (source-path (fs (build-path "../example/lain.png")))
                (build-path "../example/lain.png"))

  (check-equal? (source-path (fs "../example/lain.png"))
                (build-path "../example/lain.png")))

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
