#lang racket/base

(provide configure)

(require (only-in rackjure/lambda-reader make-lambda-readtable current-syntax-introducer))

(define (configure data)
  (current-syntax-introducer (make-syntax-introducer))
  (current-readtable (make-lambda-readtable (current-readtable))))
