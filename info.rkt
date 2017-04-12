#lang setup/infotab
(define version "0.9")
(define collection 'multi)
(define deps '("base"
               ["racket" "6.0"]
               "rackunit-lib"
               ["threading" "1.1"]))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
