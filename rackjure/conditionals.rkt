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
  (require rackunit)
  ;; Setting `current-namespace` is required for this to work with
  ;; Racket 5.3.2, although not for later versions like 5.3.5.
  (define-namespace-anchor anchor)
  (define (check-expansion input expected-output)
    (parameterize ([current-namespace (namespace-anchor->namespace anchor)])
      (check-equal? (syntax->datum (expand-once input))
                    (syntax->datum expected-output))))
  (check-expansion #'(if-let [x #t] 0 1)   #'(let [(x #t)] (if x 0 1)))
  (check-expansion #'(when-let [x #t] 0 1) #'(let [(x #t)] (when x 0 1)))
  (check-expansion #'(if-not #t 0 1)       #'(if (not #t) 0 1))
  (check-expansion #'(when-not #t 0 1)     #'(when (not #t) 0 1)))
