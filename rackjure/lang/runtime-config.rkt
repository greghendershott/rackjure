#lang racket/base

(provide configure)

(require (only-in rackjure/lambda-reader make-lambda-readtable))

(define (configure data)
  (current-readtable (make-lambda-readtable (current-readtable))))
