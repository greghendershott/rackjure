#lang racket/base

(provide alist alist?)

(require racket/match)

(define (alist . xs)
  (match xs
    [(list) (list)]
    [(list k v more ...) (cons (cons k v) (apply alist more))]
    [(list x) (raise-arity-error 'alist (arity-at-least 2) x)]))

(define (alist? xs)
  (and (list? xs)
       (for/and ([x (in-list xs)])
         (pair? x))))
