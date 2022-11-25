;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(provide partial
         box-swap!)

(module+ test
  (require rackunit))

(define ((partial f . args1) . args2)
  (apply f (append args1 args2)))

(module+ test
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
  (require racket/future)
  ;; Even with (module config info (define timeout 300)) this is
  ;; timing out on the pkg build server. No idea why it's taking >5
  ;; minutes, there, when it takes <10 secs on my laptop! For now,
  ;; disable this test completely, there.
  (unless (getenv "PLT_PKG_BUILD_SERVICE")
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
    (check-equal? (unbox shared) (* n-iterations n-futures))))
