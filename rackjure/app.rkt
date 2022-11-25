;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/dict
         "alist.rkt")

(provide -#%app
         alist
         alist?
         current-curly-dict)

;; Provide an alternative `#%app` to:
;;
;; [1] Implement applicable `dict?`s.
;;
;; (a) Handle a `dict?` in the first or second position.
;;
;; Downside: Adds a runtime `cond` check to normal applications of
;; arities 2 and 3.
;;
;; We can look for certain arities at compile time, but the real test
;; must be done at run time. For code size, do this in helper function
;; rather than expanding such test inline.
;;
;; One issue is how to handle the optional last argument to
;; `dict-ref`, which is the value to use if the key is not found. We
;; handle this slightly differently than `dict-ref`: 1. We use an
;; optional keyword argument #:else. This leaves arity 3 available to
;; mean `dict-set`. 2. When a default arg isn't supplied and the key
;; isn't found, `dict-ref` raises an error. Instead we return `#f`.
;; This is more convenient, especially when used with threading macros
;; ~> and ~>>. [It's smart that dict-ref lets you supply a specific
;; value to mean not-found -- because what if `#f` or 'not-found or
;; whatever could be a valid value in the `dict?`. But even smarter is
;; for the not-found behavior to be returning #f, by default, rather
;; than raising an error. That way, using #:else is required only for
;; the special case of a dict that needs to store #f values.]
;;
;; (b) Handle `(key #f)` as #f. This allows doing a `dict-has-key?`
;; over nested dicts with threading as `(~> dict 'a 'b 'c)`. Because
;; failure at any point will return #f, and we propogate the #f to the
;; end.
;;
;; [2] Expand `{k v ... ...}` as `((current-curly-dict) k v ... ...)`.
;; The current-curly-dict parameter may be e.g. `hash`, `hasheq`,
;; `alist`.

(define (maybe-dict-ref x y)
  (cond [(dict? x)      (dict-ref x y #f)]  ;(dict key)
        [(not y)        #f]                 ;(key #f) => #f
        [(dict? y)      (dict-ref y x #f)]  ;(key dict)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~v ~v)" x y)]))

(define (maybe-dict-ref/else x y #:else d)
  (cond [(dict? x)      (dict-ref x y d)]        ;(dict key #:else default)
        [(dict? y)      (dict-ref y x d)]        ;(key dict #:else default)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~v ~v #:else ~a)" x y d)]))

(define (maybe-dict-set x y z)
  (cond [(dict? x)      (dict-set x y z)] ;(dict key val)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~a ~a ~a)" x y z)]))

;; What function does `{ k v ... ... }` expand to? Can be `hash`,
;; `hasheq`, `alist`, or similar signature function that returns a
;; `dict?`.
(define current-curly-dict (make-parameter alist))

(define-syntax (-#%app stx)
  (syntax-parse stx
    ;; { key val ... ... } dict literals
    [(_ x:expr ...) #:when (eq? (syntax-property stx 'paren-shape) #\{)
     (define stxs (syntax->list #'(x ...)))
     (unless (zero? (remainder (length stxs) 2))
       (raise-syntax-error
        '|{ }|
        "expected even number of keys and values for dictionary"
        #'(x ...)
        (last stxs)))
     #'((current-curly-dict) x ...)]
    ;; Arities that might be dict applications
    ; Test the normal case first/fast
    [(_ x:expr y:expr)               #'(let ([x_ x] [y_ y])
                                         (cond [(procedure? x_) (#%app x_ y_)]
                                               [else            (maybe-dict-ref x_ y_)]))]
    [(_ x:expr y:expr #:else d:expr) #'(let ([x_ x] [y_ y] [d_ d])
                                         (cond [(procedure? x_) (#%app x_ y_ #:else d_)]
                                               [else            (maybe-dict-ref/else x_ y_ #:else d_)]))]
    [(_ x:expr y:expr z:expr)        #'(let ([x_ x] [y_ y] [z_ z])
                                         (cond [(procedure? x_) (#%app x_ y_ z_)]
                                               [else            (maybe-dict-set x_ y_ z_)]))]
    ;; Else just the usual Racket #%app
    [(_ f      a ...)                #'(#%app f a ...)]))
