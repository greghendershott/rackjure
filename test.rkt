#lang rackjure

;; Tests of #%app (not convenient to put in app.rkt).

(module+ test
  (require rackunit)

  (check-true ((hasheq 'a #t) 'a))
  (check-true ('a (hasheq 'a #t)))

  ;; This works because vector? is-a-kind-of dict?
  (define v (vector 0 1 2 3))
  (check-equal? ((vector 0 1 2) 1) 1)
  (check-equal? (1 (vector 0 1 2)) 1)

  (require "threading.rkt")
  (check-true (--> (hasheq 'a (hasheq 'b (hasheq 'c #t)))
                   'a
                   'b
                   'c))

  ;; A chain of dict-has-key?
  (check-equal? (--> {'a {'b {'c 0}}} 'a 'b 'c) 0)
  (check-false (--> {'a {'b {'c 0}}} 'a 'b 'huh?))
  (check-false (--> {'a {'b {'c 0}}} 'huh? 'b 'c))

  (check-equal?
   {'key "value"
         'request {'version 1.0
                   'headers {'Content-Type "foo"
                             'Content-Length 10}}
         'response {'version 1.0
                    'headers {'Content-Type "foo"
                              'Content-Length 10}}}
   (hash 'key "value"
         'request (hash 'version 1.0
                   'headers (hash 'Content-Type "foo"
                             'Content-Length 10))
         'response (hash 'version 1.0
                    'headers (hash 'Content-Type "foo"
                              'Content-Length 10))))
  )
