#lang racket

(require "alist.rkt"
         "app.rkt"
         "conditionals.rkt"
         "dict.rkt"
         "str.rkt"
         "threading.rkt"
         "utils.rkt")

(provide (except-out (all-from-out racket) #%app #%module-begin)
         (rename-out [-#%app #%app]
                     [-#%module-begin #%module-begin])
         (except-out (all-from-out "app.rkt") -#%app)
         (all-from-out "alist.rkt"
                       "conditionals.rkt"
                       "dict.rkt"
                       "str.rkt"
                       "threading.rkt"
                       "utils.rkt"))

(define-syntax-rule (-#%module-begin form ...)
  (#%module-begin
   (require (only-in rackjure/lambda-reader make-lambda-readtable))
   (current-readtable (make-lambda-readtable (current-readtable)))
   form ...))
