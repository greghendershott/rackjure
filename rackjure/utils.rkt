#lang racket/base

(provide partial)

(define ((partial f . args1) . args2)
  (apply f (append args1 args2)))

(module+ test
  (require rackunit)
  
  ;; If we tested against the variable-arity `+` there would
  ;; be no difference between `partial` and `curry`.
  (define (+* x y) (+ x y))
  
  (check-equal? ((partial +*) 1 2) 3)
  (check-equal? ((partial +* 1) 2) 3)
  (check-equal? ((partial +* 1 2)) 3))
