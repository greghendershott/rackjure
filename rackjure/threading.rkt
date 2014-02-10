#lang racket/base

(provide ~> ~>> some~> some~>>)

(require (for-syntax racket/base syntax/stx syntax/parse))
(require (only-in "conditionals.rkt" if-let))

(module+ test
  (require "check-expansion.rkt")
  (define-namespace-anchor anchor))

;; Clojure threading macros. Original versions courtesy of Asumu
;; Takikawa.
;;
;; Rewritten with help from Sam Tobin-Hochstadt to use whatever #%app
;; is bound in the lexical context of the macro usage. That way, these
;; will expand to the default Racket #%app, or to the applicative
;; dictionary #%app from app.rkt that's part of #lang rackjure, or to
;; whatever other #%app is bound at the macro usage site. This is
;; handled by the keep-stxctx combinator.
;;
;; Among other things this allows using `(require rackjure/threading)`
;; to get ~> and ~>> without the applicative dict overhead associated
;; with #lang rackjure.
;;
;; Also rewritten to use a fold instead of recursive invocations to
;; avoid the issues described in CLJ-1121.

(begin-for-syntax 
  (define stx-coerce-to-list
    (syntax-parser
     [((~literal quote) x) #'((quote x))]
     ;; ^ We want to end up with ((quote x) y), NOT (quote x y).
     [(form ...) #'(form ...)]
     [ form      #'(form)]))

  (define ((keep-stxctx f) stx . args)
    (datum->syntax stx (syntax-e (apply f stx args))))

  (define (threading-syntax-parser threader)
    (syntax-parser
     [(_ first rest ...)
      (define normalized-rest (stx-map (keep-stxctx stx-coerce-to-list)
                                       #'(rest ...)))
      (foldl (keep-stxctx threader) #'first normalized-rest)])))

(module+ test
  (define-syntax test-threader
    (threading-syntax-parser
     (lambda (form nested-form) #`(#,form . #,nested-form))))
   
  (check-expand-once anchor #'(test-threader 1) #'1)
  (check-expand-once anchor #'(test-threader 1 f (g 2)) #'((g 2) (f) . 1)))

(define-syntax ~>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f #,nested-form r ...)]))))

(define-syntax ~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f r ... #,nested-form)]))))

(define-syntax some~>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~> tmp #,form) #f))))

(define-syntax some~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~>> tmp #,form) #f))))

(module+ test
  (check-expand-once anchor #'(~> 1 (f 2))     #'(f 1 2))
  (check-expand-once anchor #'(~> #t (if 1 2)) #'(if #t 1 2))
  ;; ^ Check that it works with syntax forms
  
  (check-expand-once  anchor #'(~>> 1 (f 2))       #'(f 2 1))
  (check-expand-fully anchor #'(~>> 1 + (~>> 1 +)) #'(#%app + '1 (#%app + '1)))
  ;; ^ Example from CLJ-1121
  
  (check-expand-once anchor
                     #'(some~> 1 f)
                     #'(if-let [tmp 1](~> tmp (f)) #f))
  
  (check-expand-once anchor
                     #'(some~>> 1 f)
                     #'(if-let [tmp 1](~>> tmp (f)) #f)))

;; Confirm expansion using default #%app
(module+ test
  (module test-plain-app racket/base
    (require (submod ".." "..")) ;; for ~>
    (require "check-expansion.rkt")
    (define-namespace-anchor anchor)
    ;; 1. Directly; expanding ~> macro
    (check-expand-fully anchor
                        #'(~> 1 +)
                        #'(#%app + (quote 1)))
    ;; 2. Indirectly; no implicit require of wrong #%app
    (check-expand-fully anchor
                        #'((hasheq 'a 42) 'a)
                        #'(#%app (#%app hasheq (quote a) (quote 42))
                                 (quote a))))
  (require 'test-plain-app))

;; Confirm expansion using our applicative dict #%app
(module+ test
  (module test-dict-app racket/base
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (require "check-expansion.rkt")
    (define-namespace-anchor anchor)
    ;; 1. Directly; expanding ~> macro
    (check-expand-fully anchor
                        #'(~> 1 +)
                        #'(#%app maybe-dict-ref + (quote 1)))
    ;; 2. Indirectly; no implicit require of wrong #%app
    (check-expand-fully anchor
                        #'((hasheq 'a 42) 'a)
                        #'(#%app maybe-dict-ref
                                 (#%app maybe-dict-set
                                        hasheq (quote a) (quote 42)) (quote a))))
  (require 'test-dict-app))
