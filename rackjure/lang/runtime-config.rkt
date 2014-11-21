#lang racket/base

(provide configure)

(require (only-in rackjure/lambda-reader make-lambda-readtable))

(define (configure data)
  (define old-read (current-read-interaction))
  (define (new-read src in)
    (parameterize ([current-readtable (make-lambda-readtable (current-readtable))])
      (old-read src in)))
  (current-read-interaction new-read))
