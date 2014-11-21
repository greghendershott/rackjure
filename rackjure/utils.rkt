#lang racket/base

(require version/utils
         ;; For backward compatibility, provide `partial`
         (only-in "function.rkt" partial))

(provide box-swap!
         partial)

(module+ test
  (require rackunit))

(define (-box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))

(define (racket-5.92+?)
  (version<=? "5.92" (version)))

(define box-swap!
  (cond [(racket-5.92+?) -box-swap!]
        [(lambda _ (error 'box-swap! "requires Racket 5.92 or newer"))]))

(module+ test
  (require racket/future)
  (cond [(racket-5.92+?)
         (define shared (box 0))
         (define n-iterations 10000000)
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
         (check-equal? (unbox shared) (* n-iterations n-futures))]
        [else
         (check-exn exn:fail? (lambda () (box-swap! (box 0) + 1)))]))
