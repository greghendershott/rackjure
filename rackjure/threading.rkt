#lang racket/base

(provide ~> ~>>)

(require (for-syntax racket/base syntax/stx syntax/parse))

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
       (define normalized-rest (stx-map (keep-stxctx stx-coerce-to-list) #'(rest ...)))
       (foldl (keep-stxctx threader) #'first normalized-rest)])))

(define-syntax ~>
  (threading-syntax-parser
    (lambda (form nested-form)
      (syntax-parse form [(f r ...) #`(f #,nested-form r ...)]))))

(define-syntax ~>>
  (threading-syntax-parser
    (lambda (form nested-form)
      (syntax-parse form [(f r ...) #`(f r ... #,nested-form)]))))

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
  (check-equal? (~>> 1 + (~>> 1 +)) ;; see CLJ-1121
                 2)
  ;; Confirm expansion using default #%app
  (module plain racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    (let ([d (hasheq 'a 42)])
      ;; these should NOT work without custom #%app
      (check-exn exn:fail? (lambda () (d 'a)))
      (check-exn exn:fail? (lambda () (~> 'a d)))
      (check-exn exn:fail? (lambda () ('a d)))
      (check-exn exn:fail? (lambda () (~> d 'a)))))
  (require 'plain)
  ;; Confirm expansion using applicative dict #%app
  (module dict racket/base
    (require rackunit)
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (check-equal? (~> 1 (+ 1) (* 2) (+ 1))
                  5)
    ;; these SHOULD work with custom #%app
    (let ([d (hasheq 'a 42)])
      (check-equal? (d 'a) 42)
      (check-equal? (~> 'a d) 42)
      (check-equal? ('a d) 42)
      (check-equal? (~> d 'a) 42))
    ;; Nested using ~> (threading macro)
    (check-equal? (~> (hasheq 'a (hasheq 'b (hasheq 'c 42)))
                      'a 'b 'c)
                  42))
  (require 'dict)
  ;; Confirm still works with syntax forms as well as function #%app.
  (check-true (~> #t (if #t #f))))
