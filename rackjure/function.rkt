#lang racket/base

(require racket/match)

(provide partial
         partition
         drop
         take
         juxt
         every?
         some)

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

;; Clojure-style `partition` (not at all like Racket's).
;;
;; "Returns a lazy sequence of lists of n items each, at offsets step
;; apart. If step is not supplied, defaults to n, i.e. the partitions
;; do not overlap. If a pad collection is supplied, use its elements
;; as necessary to complete last partition upto n items. In case there
;; are not enough padding elements, return a partition with less than
;; n items."
(define partition
  (match-lambda*
    [(list n xs) (partition n n xs)]
    [(list n step (list)) (list)]
    [(list n step xs)
     (define ys (take xs n))
     (cond [(= (length ys) n) (cons ys (partition n step (drop xs step)))]
           [else '()])]
    [(list n step pad (list)) (list)]
    [(list n step pad xs)
     (define ys (take xs n))
     (cond [(= (length ys) n) (cons ys (partition n step pad (drop xs step)))]
           [else (list (take (append ys pad) n))])]))

(module+ test
  ;; 2-arg variant
  (check-equal? (partition 2 '(1 2 3 4 5 6))
                '((1 2) (3 4) (5 6)))
  ;; 3-arg variant: Explicit step
  (check-equal? (partition 2 2 '(1 2 3 4 5 6))
                '((1 2) (3 4) (5 6)))
  (check-equal? (partition 2 1 '(1 2 3 4 5 6))
                '((1 2) (2 3) (3 4) (4 5) (5 6)))
  ;; 4-arg variant: Explicit step, and, pad
  (check-equal? (partition 3 3 '(pad pad pad) '(1 2 3 4 5 6 7))
                '((1 2 3) (4 5 6) (7 pad pad))))

;; Like Racket `drop` but OK if list has fewer than `n` members.
(define (drop xs n)
  (cond [(and (> n 0) (not (null? xs))) (drop (cdr xs) (sub1 n))]
        [else xs]))

(module+ test
  (let ([xs '(1 2 3 4)])
    (check-equal? (drop xs -1) '(1 2 3 4))
    (check-equal? (drop xs 0) '(1 2 3 4))
    (check-equal? (drop xs 4) '())
    (check-equal? (drop xs 5) '())
    (check-equal? (drop xs 2) '(3 4))))

;; Like `take`, but OK if list has fewer than `n` members.
(define (take xs n)
  (for/list ([x (in-list xs)]
             [_ (in-range n)])
    x))

;; "Takes a set of functions and returns a fn that is the juxtaposition
;; of those fns.  The returned fn takes a variable number of args, and
;; returns a vector containing the result of applying each fn to the
;; args (left-to-right).
;; ((juxt a b c) x) => [(a x) (b x) (c x)]"
(define ((juxt . fs) . args)
  (for/list ([f (in-list fs)])
    (apply f args)))

(module+ test
  (check-equal? ((juxt add1 sub1 + -) 10) '(11 9 10 -10)))

;; `every?` is just an alias for `andmap`.
(define every? andmap)

;; Unlike `ormap`, which returns #t or #f, `some` returns the first
;; item satisfying ?, else #f. i.e. Like `for/for`.
(define (some ? xs)
  (for/or ([x (in-list xs)])
    (if (? x) x #f)))

(module+ test
  (check-equal? (some even? '(2 4)) 2))

(define (frequencies xs)
  (for/fold ([ht (hash)])
            ([x (in-list xs)])
    (hash-update ht x (λ (v) (add1 v)) 0)))

(module+ test
  (check-equal? (frequencies '(1
                               2 2
                               3 3 3))
                #hash((1 . 1) (2 . 2) (3 . 3)))
  (check-equal? (frequencies '())
                #hash()))
