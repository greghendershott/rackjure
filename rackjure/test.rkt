#lang rackjure

;;; Tests of #%app (not convenient to put in app.rkt).

(module+ test
  (require rackunit
           syntax/strip-context)

  ;; Application with the `dict` in first or second position
  (define d (hasheq 'a #t))
  (check-true (d 'a))
  (check-true ('a d))

  ;; vector is-a-kind-of dict
  (define v (vector 0 1 2 3))
  (check-equal? (v 1) 1)
  (check-equal? (1 v) 1)

  ;; Nested using ~> (threading macro)
  (check-equal? (~> (hasheq 'a (hasheq 'b (hasheq 'c 42)))
                    'a
                    'b
                    'c)
                42)

  ;; Nested dict-ref
  (check-equal? (~> {'a {'b {'c 0}}} 'a 'b 'c) 0)

  ;; Nested dict-has-key?
  (check-false (~> {'a {'b {'c 0}}} 'a 'b 'huh?))
  (check-false (~> {'a {'b {'c 0}}} 'huh? 'b 'c))

  ;; {} default `alist`
  (check-equal?
   {'key "value"
    'request {'version 1.0
              'headers {'Content-Type "foo"
                        'Content-Length 10}}
    'response {'version 1.0
               'headers {'Content-Type "foo"
                         'Content-Length 10}}}
   (alist 'key "value"
          'request (alist 'version 1.0
                          'headers (alist 'Content-Type "foo"
                                          'Content-Length 10))
          'response (alist 'version 1.0
                           'headers (alist 'Content-Type "foo"
                                           'Content-Length 10))))

  ;; {} using `current-curly-dict` parameter to specify `hasheq`
  (check-equal?
   (parameterize ([current-curly-dict hasheq])
     {'key "value"
      'request {'version 1.0
                'headers {'Content-Type "foo"
                          'Content-Length 10}}
      'response {'version 1.0
                 'headers {'Content-Type "foo"
                           'Content-Length 10}}})
   (hasheq 'key "value"
           'request (hasheq 'version 1.0
                            'headers (hasheq 'Content-Type "foo"
                                             'Content-Length 10))
           'response (hasheq 'version 1.0
                             'headers (hasheq 'Content-Type "foo"
                                              'Content-Length 10))))

  ;; {} with odd number of elements raises exn:fail:syntax
  #;(check-exn exn:fail?
             (λ _
               (parameterize ([current-namespace (make-base-namespace)])
                 (eval (namespace-syntax-introduce
                        (strip-context
                         #'(module m rackjure {0 1 2}))))))
             "expected even number of keys and values for dictionary"))

;;; Tests of lambda reader macro not convenient to put in lambda-reader.rkt

(module+ test
  ;; Using #λ( ... )
  (check-equal? (map #λ(+ % 1) '(1 2 3))
                '(2 3 4))
  (check-equal? (map #λ(+ % %2) '(1 2 3) '(1 2 3))
                '(2 4 6))
  (check-equal? (#λ(apply list* % %&) 1 '(2 3))
                '(1 2 3))
  ;; Using #lambda( ... )
  (check-equal? (map #lambda(+ % 1) '(1 2 3))
                '(2 3 4))
  (check-equal? (map #lambda(+ % %2) '(1 2 3) '(1 2 3))
                '(2 4 6))
  (check-equal? (#lambda(apply list* % %&) 1 '(2 3))
                '(1 2 3))
  ;; Using #fn( ... )
  (check-equal? (map #fn(+ % 1) '(1 2 3))
                '(2 3 4))
  (check-equal? (map #fn(+ % %2) '(1 2 3) '(1 2 3))
                '(2 4 6))
  (check-equal? (#fn(apply list* % %&) 1 '(2 3))
                '(1 2 3))
  ;; #fn doesn't interfere with #f
  (check-equal? #f #f)
  (check-false #f)
  ;; The examples from PR #38
  (check-equal? (map #λ(+ % 1) '(1 2 3))
                '(2 3 4))
  (check-equal? (map #λ(+ % %2) '(1 2 3) '(1 2 3))
                '(2 4 6))
  (check-equal? (#λ(apply list* % %&) 1 '(2 3))
                '(1 2 3))
  (check-equal? (#λ(* 1/2 %#:m (* %#:v %#:v)) #:m 2 #:v 1)
                1)        ;keyword-arguments
  (let ([x (#λ"I am x")])
    (check-equal? (#λx) "I am x")) ;the body doesn't have to be in parens
  (check-equal? (#λ(+ % %1) 2)
                4)        ;% means exactly the same as %1, and you can
                          ;even use both at the same time ...
  (check-equal? (#λ(begin (set! % "%") %1) "%1")
                "%")      ;...and even set!-ing one set!s the other.
  (check-equal? (#λ(begin %2) "ignored" "used")
                "used")       ;handles skipped arguments
  (check-equal? (apply #λ(list %1 %42) (build-list 42 add1))
                (list 1 42))  ;handles an arbitrary number of arguments
  (check-equal? (let ([lambda "not lambda"] [define-syntax "not define-syntax"])
                  (#λ(+ % 1) 0))
                1) ; lambda literals should work even if `lambda` is shadowed
  )
