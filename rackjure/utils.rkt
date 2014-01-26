#lang racket/base

(provide partial box-swap!)

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

(define (box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))

(module+ test
  (require rackunit)
  (require racket/future)
  
  (define shared (box 0))
  (define n-iterations 1000)
  (define n-futures 10)
  
  (define (futures)
    (define (thunk)
      (for ([_ (in-range n-iterations)])
        ;; Use `+ 1` instead of `add1` to exercise `box-swap!`
        (box-swap! shared + 1)))
    
    (for/list ([_ n-futures])
      (future thunk)))
  
  (for ([f (futures)])
    (touch f))
  
  (check-equal? (unbox shared) (* n-iterations n-futures)))
