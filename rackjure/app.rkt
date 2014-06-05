#lang racket/base

;; Redefine #%app to:
;;
;; [1] Handle a dict in the first or second position.
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
;; optional _keyword_ argument #:else. This leaves arity 3 available
;; to mean `dict-set`. 2. If this arg isn't supplied and the key isn't
;; found, `dict-ref` raises an error. Instead we return `#f`. This is
;; a more convenient, espeically when used with threading macros -->
;; and -->>. It's smart that dict-ref lets you supply a specific value
;; to mean not-found, because what if `#f` or 'not-found or whatever
;; could be a valid value in the dict?  But even it's smarter to have
;; some default not-found other than error, and make that be #f.  That
;; way, burden is only on code that needs to store #f as values in a
;; dict, to use the #:else keyword.
;;
;; [1](a) Handle (key #f) as #f. This allows doing a `dict-has-key?`
;; over nested dicts with threading as (--> dict 'a 'b 'c). Because
;; failure at any point will return #f, and we propogate the #f to the
;; end.
;;
;; [2] Expand {k v ... ...} as (dict k v ... ...).

(provide -#%app
         alist
         alist?
         current-curly-dict)

(require (for-syntax racket/base
                     syntax/parse
                     racket/list)
         racket/dict
         "alist.rkt")

(define (maybe-dict-ref x y)
  (cond [(procedure? x) (#%app    x y)]     ;check normal case first/fast
        [(dict? x)      (dict-ref x y #f)]  ;(dict key)
        [(not y)        #f]                 ;(key #f) => #F
        [(dict? y)      (dict-ref y x #f)]  ;(key dict)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~v ~v)" x y)]))

(define (maybe-dict-ref/else x y #:else d)
  (cond [(procedure? x) (#%app    x y #:else d)] ;check normal case first/fast
        [(dict? x)      (dict-ref x y d)]        ;(dict key #:else default)
        [(dict? y)      (dict-ref y x d)]        ;(key dict #:else default)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~v ~v #:else ~a)" x y d)]))

(define (maybe-dict-set x y z)
  (cond [(procedure? x) (#%app    x y z)] ;normal case first/fast
        [(dict? x)      (dict-set x y z)] ;(dict key val)
        [else (error 'applicable-dict
                     "No dict? supplied\nin: (~a ~a ~a)" x y z)]))

;; What does {} use to initialize? Can be `hash`, `hasheq`, `alist`,
;; or similar signature function that returns a `dict?`.
(define current-curly-dict (make-parameter alist))

(define-syntax (-#%app stx)
  (syntax-parse stx
    ;; { ... } dict literals
    [(_ x:expr ...) #:when (eq? (syntax-property stx 'paren-shape) #\{)
     (unless (zero? (remainder (length (syntax->list #'(x ...))) 2))
       (raise-syntax-error '|{ }|
                           "expected even number of items for dictionary"
                           (datum->syntax #f (cdr (syntax->list stx)) stx stx)
                           (last (syntax->list #'(x ...)))))
     #'((current-curly-dict) x ...)]
    ;; Arities that might be dict applications
    [(_ x:expr y:expr)               #'(maybe-dict-ref x y)]
    [(_ x:expr y:expr #:else d:expr) #'(maybe-dict-ref/else x y #:else d)]
    [(_ x:expr y:expr z:expr)        #'(maybe-dict-set x y z)]
    ;; The usual
    [(_ f      a ...)                #'(#%app f a ...)]))
