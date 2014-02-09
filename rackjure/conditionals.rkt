#lang racket/base

(provide if-let when-let if-not when-not)

(require syntax/parse/define)

(define-simple-macro (if-let [binding:id value:expr] then:expr else:expr)
  (let ([binding value])
    (if binding then else)))

(define-simple-macro (when-let [binding:id value:expr] body:expr ...+)
  (let ([binding value])
    (when binding body ...)))

(define-simple-macro (if-not test:expr then:expr else:expr)
  (if (not test) then else))

(define-simple-macro (when-not test:expr body:expr ...+)
  (when (not test) body ...))

(module+ test
  (require rackunit
           "check-expansion.rkt")
  (define-namespace-anchor a)
  (check-expand-once a #'(if-let [x #t] 0 1)   #'(let [(x #t)] (if x 0 1)))
  (check-expand-once a #'(when-let [x #t] 0 1) #'(let [(x #t)] (when x 0 1)))
  (check-expand-once a #'(if-not #t 0 1)       #'(if (not #t) 0 1))
  (check-expand-once a #'(when-not #t 0 1)     #'(when (not #t) 0 1)))
