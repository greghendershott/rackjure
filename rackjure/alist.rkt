;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(provide alist alist?)

(require racket/match)

(define (alist . xs)
  (match xs
    [(list* k v more) (cons (cons k v) (apply alist more))]
    [(list x)         (raise-arity-error 'alist (arity-at-least 2) x)]
    [(list)           (list)]))

(define (alist? xs)
  (and (list? xs)
       (for/and ([x (in-list xs)])
         (pair? x))))

(module+ test
  (require rackunit)
  (check-equal? (alist 1 2 3 4 5 6) '([1 . 2][3 . 4][5 . 6]))
  (check-equal? (alist) '()))
