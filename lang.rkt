#lang racket
(provide
 #%module-begin
 (all-from-out racket)
 (all-from-out musiclibrary))

(require
 musiclibrary
 "./private/entry-point.rkt"
 (for-syntax
  racket/base
  syntax/parse))

;; ---------------------------------------------------------------------------------------

(module reader syntax/module-reader
  musiclibrary/lang)

;; ---------------------------------------------------------------------------------------

;; module-begin wrapper for #lang musiclibrary
(define-syntax #%module-begin
  (syntax-parser
    [(_ body ...)
     #'(#%plain-module-begin
        (traverse-module () body ...))]))

;; (traverse-module [tmp-id ...] form ...)
;;
;; traverses every form, replacing toplevel expressions with (define tmp-id <expr>), and
;; accumulates these tmp-id's. inserts (finish tmp-id ...) at the end of the module.

;; TODO: look into syntax-local-lift + wrapping-module-begin

(begin-for-syntax
  (define-syntax-class non-expression-form
    [pattern ({~or {~literal define-values}
                   {~literal define-syntaxes}
                   {~literal module}
                   {~literal module*}
                   {~literal #%require}
                   {~literal #%provide}}
              . _)]))

(define-syntax traverse-module
  (syntax-parser
    [(_ acc)
     #'(finish . acc)]

    [(_ acc form0 . forms)
     (syntax-parse (local-expand #'form0 'module #f)
       ; splice (begin ..)'s
       [({~literal begin} form* ...)
        #'(traverse-module acc form* ... . forms)]

       ; leave non-expressions alone
       [form:non-expression-form
        #'(begin
            form
            (traverse-module acc . forms))]

       ; accumulate expressions
       [expr
        #:with [tmp-id*] (generate-temporaries #'[expr])
        #:with acc* #'[tmp-id* . acc]
        #'(begin
            (define tmp-id* expr)
            (traverse-module acc* . forms))])]))

(define-syntax finish
  (syntax-parser
    [(_ id ...)
     #'(generate-music-library
        (pick-out-albums id ...))]))

;; any ... -> (listof album)
(define (pick-out-albums . things)
  (define maybe-albums
    (for/list ([thing (in-list things)])
      (cond
        [(void? thing) #f]
        [(track? thing) (album/single thing)]
        [(album? thing) thing]
        [else (error 'musiclibrary
                     (~a "invalid toplevel expression.\n"
                         "  expected: (or/c track? album? void?)\n"
                         "  got: " thing))])))

  (filter values maybe-albums))
