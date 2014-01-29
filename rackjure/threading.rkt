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

;; The rewrite somewhat obfuscates the logic. When reading the code
;; below, keep in mind that `op` means the ~> or ~>> used to invoke
;; the macro, and #,(datum->syntax #'op '#%app) means "use whatever
;; #%app is bound to in the lexical context of `op`". Whenever the
;; macro recursively expands to another pattern variation of itself,
;; it is careful to use `op` to pass through and preserve the original
;; lexical context.

(define-syntax (~> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    [(op x ((~literal quote) e))   ;want ((quote e) x), NOT (quote e a)
     #`(#,(datum->syntax #'op '#%app) (quote e) x)] ;((quote e) x)
    [(op x (e e_1 ...))
     #`(#,(datum->syntax #'op '#%app) e x e_1 ...)] ;(e x e_1 ...)
    [(op x e)
    #`(op x (e))]                       ;(~> x (e))
    [(op x form form_1 ...)
     #'(op (op x form) form_1 ...)]))   ;(~> (~> x form) form_1 ..)

(define-syntax (~>> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    [(op x ((~literal quote) e))   ;want ((quote a) x), NOT (quote x a)
     #`(#,(datum->syntax #'op '#%app) (quote e) x)] ;((quote e) x)
    [(op x (e e_1 ...))
     #`(#,(datum->syntax #'op '#%app) e e_1 ... x)] ;(e e_1 ... x)
    [(op x e)
     #'(op x (e))]                      ;(~>> x (e)
    [(op x form form_1 ...)
     #'(op (op x form) form_1 ...)]))   ;(~>> (~>> x form) form_1 ...)

(module+ test
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
    (check-equal? (syntax->datum (expand #'(+ 0)))
                  '(#%app + '0)))
  (require 'plain)
  ;; Confirm expansion using applicative dict #%app
  (module dict racket/base
    (require rackunit)
    (require (rename-in "app.rkt" [-#%app #%app]))
    (check-equal? (syntax->datum (expand #'(+ 0)))
                  '(#%app maybe-dict-ref + '0)))
  (require 'dict))
