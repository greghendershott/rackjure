#lang racket/base

(provide if-let when-let)

(require syntax/parse/define)

(define-simple-macro (if-let [binding:id value:expr] then:expr else:expr)
  (let ([binding value])
    (if binding then else)))

(define-simple-macro (when-let [binding:id value:expr] body:expr ...+)
  (let ([binding value])
    (when binding body ...)))

(module+ test
  (require rackunit)
  
  (check-equal? (if-let [x #t] 0 1) 0)
  (check-equal? (if-let [x #f] 0 1) 1)
  (check-equal? (if-let [x #t] x 0) #t)
  
  (check-equal? (when-let [x #t] 0 1) 1)
  (check-equal? (when-let [x #f] 0 1) (void))
  (check-equal? (when-let [x #t] x) #t))
