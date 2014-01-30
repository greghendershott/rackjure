#lang racket/base

(provide ~> ~>>)

(require (for-syntax racket/base syntax/parse))

;; Clojure threading macros. Original versions courtesy of Asumu
;; Takikawa.
;;
;; Rewritten with help from Sam Tobin-Hochstadt to use whatever #%app
;; is bound in the lexical context of the macro usage. That way, these
;; will expand to the default Racket #%app, or to the applicative
;; dictionary #%app from app.rkt that's part of #lang rackjure, or to
;; whatever other #%app is bound at the macro usage site.
;;
;; Among other things this allows using `(require rackjure/threading)`
;; to get ~> and ~>> without the applicative dict overhead associated
;; with #lang rackjure.

;; The rewrite somewhat obfuscates the original logic. When reading
;; the code below, keep in mind that:
;;
;; - `op` means the ~> or ~>> used to invoke the macro. We need its
;;   lexical context!
;;
;; - Whenever the macro recursively expands to another pattern
;;   variation of itself, it is careful to use `op` to pass through
;;   and preserve the original lexical context. We need it!
;;
;; - On the terminal patterns, the macro needs to use the lexical
;;   context of `op` for "the parens" -- for the cons or list -- of
;;   the expanded form. For example, instead of a template like:
;;
;;       #'(e x e_1 ...)
;;
;;   it must be:
;;
;;       (datum->syntax #'op (cons #'e #'(x e_1 ...))).
;;
;;   This correctly handles both cases, where `e` is a function
;;   binding (a "function call") and where it is a transformer binding
;;   (a "macro call").
;;
;;   - If the form is a function application, the expander will use
;;     whatever #%app is bound in the lexical context where the macro
;;     is used.
;;
;;   - If the form uses a transformer binding (it is a "macro call"),
;;     the expander will handle it as usual. (An earlier version of
;;     this always injected an #%app, which prevented ~> and ~>>
;;     continuing to work with macros.)

(define-syntax (~> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    [(op x ((~literal quote) e))   ;want ((quote e) x), NOT (quote e x)
     (datum->syntax #'op (list #'(quote e) #'x))] ;#'((quote e) x)
    [(op x (e e_1 ...))
     (datum->syntax #'op (cons #'e #'(x e_1 ...)))] ;#'(e x e_1 ...)
    [(op x e)
    #`(op x (e))]                       ;(~> x (e))
    [(op x form form_1 ...)
     #'(op (op x form) form_1 ...)]))   ;(~> (~> x form) form_1 ..)

(define-syntax (~>> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    [(op x ((~literal quote) e))   ;want ((quote a) x), NOT (quote x a)
     (datum->syntax #'op (list #'(quote e) #'x))] ;#'((quote e) x)
    [(op x (e e_1 ...))
     (datum->syntax #'op (cons #'e #'(e_1 ... x)))] ;#'(e e_1 ... x)
    [(op x e)
     #'(op x (e))]                      ;(~>> x (e)
    [(op x form form_1 ...)
     #'(op (op x form) form_1 ...)]))   ;(~>> (~>> x form) form_1 ...)

(module* test racket/base
  (require (submod ".."))
  (require rackunit
           (only-in racket/string string-split string-replace))
  (check-equal? (~> "a b c d"
                    string-upcase
                    (string-replace "A" "X")
                    (string-split " ")
                    car)
                "X")
  (check-equal? (~>> 5 (+ 3) (/ 2) (- 1))
                (/ 3 4))
  (check-equal? (~>> 1 add1)
                2)
  ;; Confirm expansion using default #%app
  (module plain racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    (define expand/datum (compose1 syntax->datum expand))
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    (check-equal? (expand/datum #'(+ 1 1))
                  '(#%app + '1 '1))
    (check-equal? (expand/datum #'(~> 1 (+ 1)))
                  '(#%app + '1 '1)))
  (require 'plain)
  ;; Confirm expansion using applicative dict #%app
  (module dict racket/base
    (require (submod ".." "..")) ;; for ~>
    (require rackunit)
    (require (rename-in "app.rkt" [-#%app #%app]))
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    (let ([d (hasheq 'a 42)])
      (check-equal? (d 'a) 42)
      (check-equal? (~> 'a d) 42)
      (check-equal? ('a d) 42)
      (check-equal? (~> d 'a) 42)
      )
    ;; Nested using ~> (threading macro)
    (check-equal? (~> (hasheq 'a (hasheq 'b (hasheq 'c 42)))
                      'a 'b 'c)
                  42)
    (define expand/datum (compose1 syntax->datum expand))
    (check-equal? (expand/datum #'(+ 1 1))
                  '(#%app maybe-dict-set + '1 '1))
    (check-equal? (expand/datum #'(~> 1 (+ 1)))
                  '(#%app maybe-dict-set + '1 '1)))
  (require 'dict)
  ;; Confirm still works with syntax forms as well as function #%app.
  (check-true (~> #t (if #t #f))))
