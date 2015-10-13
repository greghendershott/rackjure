#lang setup/infotab
(define version "0.8")
(define collection 'multi)
(define deps '(["racket" "6.0"]
               "base"
               "rackunit-lib"
               ["threading" "1.1"]))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
