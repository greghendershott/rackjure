#lang racket/base

;; Originally this had its own implementation. Now we use depend on
;; the `threading` package and re-provide all of it, plus rename its
;; and~> to some~> for backward compatibility.
;;
;; Preserving this module and doing the re-provide here, for programs
;; that (require rackjure/threading).
;;
;; Preserving some of the original tests to ensure compatability
;; during the transition and going forward. (The tests that checked
;; using expand-once needed to be changed because the threading
;; package implementation's intermediate expansion steps aren't not
;; necessarily the same -- but the fully-expanded forms should be.)

(require threading)

(provide (all-from-out threading)
         (rename-out [and~>  some~>]
                     [and~>> some~>>]))

(module+ test
  (require "check-expansion.rkt")
  (define-namespace-anchor anchor)

  (check-expand-fully anchor #'(~> 1 (f 2))     #'(#%app f (quote 1) (quote 2)))
  (check-expand-fully anchor #'(~> #t (if 1 2)) #'(if (quote #t) (quote 1) (quote 2)))
  ;; ^ Check that it works with syntax forms

  (check-expand-fully anchor #'(~>> 1 (f 2))       #'(#%app f (quote 2) (quote 1)))
  (check-expand-fully anchor #'(~>> 1 + (~>> 1 +)) #'(#%app + '1 (#%app + '1)))
  ;; ^ Example from CLJ-1121


  ;; Confirm expansion using default #%app
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
                        #'(#%app (#%app hasheq (quote a) (quote 42)) (quote a))))
  (require 'test-plain-app)

  ;; Confirm expansion using our applicative dict #%app
  (module test-dict-app racket/base
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (require "check-expansion.rkt")
    (define-namespace-anchor anchor)
    ;; 1. Directly; expanding ~> macro
    (check-expand-fully/both anchor
                             #'(~> 1 +)
                             #'(#%app + (quote 1)))
    ;; 2. Indirectly; no implicit require of wrong #%app
    (check-expand-fully/both anchor
                             #'((hasheq 'a 42) 'a)
                             #'(#%app (#%app hasheq (quote a) (quote 42)) (quote a))))
  (require 'test-dict-app))
