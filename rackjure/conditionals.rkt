#lang racket/base

(provide if-let when-let)

(require (for-syntax racket/base syntax/parse))

(define-syntax (if-let stx)
  (syntax-parse stx
    [(_ [binding:id value] then-expr else-expr)
     #'(let ([binding value])
         (if binding then-expr else-expr))]))

(define-syntax (when-let stx)
  (syntax-parse stx
    [(_ [binding:id value] body ...)
     #'(let ([binding value])
         (when binding body ...))]))

(module+ test
  (require rackunit)
  
  (check-equal? (if-let [x #t] 0 1) 0)
  (check-equal? (if-let [x #f] 0 1) 1)
  (check-equal? (if-let [x #t] x 0) #t)
  
  (check-equal? (when-let [x #t] 0 1) 1)
  (check-equal? (when-let [x #f] 0 1) (void))
  (check-equal? (when-let [x #t] x) #t))
