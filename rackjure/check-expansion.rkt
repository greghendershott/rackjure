#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         rackunit)

(provide check-expand-once
         check-expand-fully
         check-equal?)

;; 1. These are macros not functions so that check failure source
;; location will be correct. Also, note we need to use quasisyntax/loc
;; specifically on the `check-equal?` form, since it's inside another
;; form and quasisyntax/loc doesn't change the source for pieces
;; inside the form.
;;
;; 2. Setting `current-namespace` is required for this to work with
;; Racket 5.3.2, although not for later versions like 5.3.5.

(begin-for-syntax
  (define-syntax-class anchor
    #:description "An identifier created with define-namespace-anchor"
    (pattern a:id)))

(define-syntax (check-expansion stx)
  (syntax-parse stx
    [(_ expander:id anchor:anchor input:expr expected:expr)
     #`(parameterize ([current-namespace (namespace-anchor->namespace anchor)])
         #,(quasisyntax/loc stx
             (check-equal? (syntax->datum (expander input))
                           (syntax->datum expected))))]))

(define-syntax-rule (check-expand-once anchor input expected)
  (check-expansion expand-once anchor input expected))

(define-syntax-rule (check-expand-fully anchor input expected)
  (check-expansion expand anchor input expected))
