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
  
  (define (check-expansion input expected-output)
    (check-equal? (syntax->datum (expand-once input))
                  (syntax->datum expected-output)))
  
  (check-expansion #'(if-let [x #t] 0 1)   #'(let [(x #t)] (if x 0 1)))
  
  (check-expansion #'(when-let [x #t] 0 1) #'(let [(x #t)] (when x 0 1))))
