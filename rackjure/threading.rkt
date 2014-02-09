#lang racket/base

(provide ~> ~>> some~> some~>>)

(require (for-syntax racket/base syntax/stx syntax/parse))
(require (only-in "conditionals.rkt" if-let))

(module+ test
  (require rackunit racket))

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

(define-syntax ~>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f #,nested-form r ...)]))))

(module+ test
  (check-equal? (~> 1 (+ 1) (* 2) (+ 1)) 5)
  (check-equal? (~> "a b c d"
                    string-upcase
                    (string-replace "A" "X")
                    (string-split " ")
                    car)
                "X")
  ;; Confirm still works with syntax forms as well as function #%app.
  (check-true (~> #t (if #t #f)))
  (require racket/match)
  (check-true (~> #t (match [#t #t][_ #f]))))

(define-syntax ~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f r ... #,nested-form)]))))

(module+ test
  (check-equal? (~>> 5 (+ 3) (/ 2) (- 1)) (/ 3 4))
  (check-equal? (~>> 1 add1) 2)
  (check-equal? (~>> 1 + (~>> 1 +)) 2)) ;example from  CLJ-1121

(define-syntax some~>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~> tmp #,form) #f))))

(define-syntax some~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~>> tmp #,form) #f))))

(module+ test
  (check-equal? (some~> 1 (/ 2)) 1/2)
  (check-equal? (some~>> 1 (/ 2)) 2)
  (check-equal? (some~> #f add1) #f)
  (check-equal? (some~>> #f add1) #f)
  (check-equal? (some~> 1 ((lambda _ #f)) add1) #f)
  (check-equal? (some~>> 1 ((lambda _ #f)) add1) #f))

;; Confirm expansion using default #%app
(module+ test
  (module test-plain-app racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (require "check-expansion.rkt")
    (define-namespace-anchor a)
    (check-expand-fully a
                        #'((hasheq 'a 42) 'a)
                        #'(#%app (hasheq (quote a) 42) (quote a))))
  (require 'test-plain-app))

;; Confirm expansion using our applicative dict #%app
(module+ test
  (module test-dict-app racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (require "check-expansion.rkt")
    (define-namespace-anchor a)
    (check-expand-fully a
                        #'((hasheq 'a 42) 'a)
                        #'(maybe-dict-ref (hasheq (quote a) 42) (quote a))))
  (require 'test-dict-app))
