#lang racket/base

(provide ~> ~>>)

(require (for-syntax racket/base syntax/parse))
(require (rename-in "app.rkt" [-#%app #%app]))

;; Clojure threading macros. This is almost all courtesy of Asumu
;; Takikawa. I did add handling of quote (symbols) to make sure it
;; worked with dict applications.

(define-syntax (~> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    ;; When e is a symbol like 'a, that's actually (quote a), so DON'T
    ;; thread x into that. We want ((quote a) x), NOT (quote x a).
    [(_ x ((~literal quote) e))
     #'((quote e) x)]
    [(_ x (e e_1 ...))
     #'(e x e_1 ...)]
    [(_ x e)
     #'(~> x (e))]
    [(_ x form form_1 ...)
     #'(~> (~> x form) form_1 ...)]))

(define-syntax (~>> stx)
  (syntax-parse stx
    [(_ x)
     #'x]
    ;; When e is a symbol like 'a, that's actually (quote a), so DON'T
    ;; thread x into that. We want ((quote a) x), NOT (quote x a).
    [(_ x ((~literal quote) e))
     #'((quote e) x)]
    [(_ x (e e_1 ...))
     #'(e e_1 ... x)]
    [(_ x form form_1 ...)
     #'(~>> (~>> x form) form_1 ...)]))

(module+ test
  (require rackunit
           (only-in racket/string string-split string-replace))
  (check-equal? (~> "a b c d"
                    string-upcase
                    (string-replace "A" "X")
                    (string-split " ")
                    car)
                "X")
  (check-equal? (~> (hasheq 'a 0)
                    'a)
                0)
  (check-equal? (~>> 5 (+ 3) (/ 2) (- 1))
                (/ 3 4))
  (check-equal? (~>> (hasheq 'a 0)
                    'a)
                0))


