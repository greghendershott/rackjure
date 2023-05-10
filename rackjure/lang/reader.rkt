#lang racket/base

(provide read read-syntax get-info)

(require (prefix-in - "reader-no-wrap.rkt")
         "../lambda-reader.rkt")

(define (read in p ln col pos) (wrapper1 (λ () (-read in p ln col pos))))
(define (read-syntax src in p ln col pos) (wrapper1 (λ () (-read-syntax src in p ln col pos))))
(define get-info -get-info)
