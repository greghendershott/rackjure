#lang info
(define version "0.9")
(define collection 'multi)
(define deps '(["base" #:version "6.1"]
               "rackunit-lib"
               ["threading" #:version "1.1"]))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
