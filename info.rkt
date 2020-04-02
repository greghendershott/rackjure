#lang info
(define version "0.10")
(define collection 'multi)
(define deps '(["base" #:version "6.3"]
               "rackunit-lib"
               ["threading-lib" #:version "1.1"]))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     "threading-doc"))
