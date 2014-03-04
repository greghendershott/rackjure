#lang rackjure

;;; Tests of #%app (not convenient to put in app.rkt).

(module+ test
  (require rackunit)

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
  (check-exn exn:fail:syntax?
             (thunk (eval #'(module m rackjure
                              {0 1 2})))
             "expected even number of items for dictionary"))

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
  (check-false #f))
