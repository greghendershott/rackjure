#lang racket/base

(require racket/contract racket/dict
         "alist.rkt")

(provide (contract-out [dict-merge (dict? dict? . -> . dict?)]
                       [dict-merge-delete-value (() (any/c) . ->* . any/c)]))

;; Instead of this:
;;
;; (define (some-middleware r)
;;   (dict-set* r
;;     'request (dict-set* (dict-ref r 'request)
;;                'version 1.1
;;                'headers (dict-set* (dict-refs r 'request 'headers)
;;                           'Content-Type "foo"
;;                           'Content-Length 10))
;;     'response (dict-set* (dict-ref r 'response)
;;                 'headers (dict-set* (dict-refs r 'request 'headers)
;;                           'Content-Type "foo"
;;                           'Content-Length 10))))
;;
;; We can write this:
;;
;; (define (some-middleware r)
;;   (dict-merge r
;;    (hasheq
;;     'request (hasheq 'version 1.1
;;                      'headers (hasheq 'Content-Type "foo"
;;                                       'Content-Length 10))
;;     'response (hasheq 'headers (hasheq 'Content-Type "foo"
;;                                        'Content-Length 10)))))

;; Functionally merge d1 into d0. Values in d0 are overriden by values
;; with the same key in d1, but otherwise values in d0
;; survive. Setting a value in d1 to 'DELETE causes it to be deleted
;; from d0 (it is not an error if it doesn't already exist in
;; d0). When a value in d1 is itself a dict?, then it is handled
;; recursively.
(define dict-merge-delete-value (make-parameter 'DELETE))
(define (dict-merge d0 d1)
  (for/fold ([d0 d0]) ([(k v) (in-dict d1)])
    (cond [(dict? v)
           (define (default d)
             (cond [(hash? d) (cond [(hash-eq? d) (hasheq)]
                                    [else (hash)])]
                   [(alist? d) '()]
                   [else (raise-type-error 'dict-merge
                                           "hash?, hasheq? or alist?"
                                           d)]))
           (dict-set d0 k (dict-merge (dict-ref d0 k (default (dict-ref d1 k)))
                                      (dict-ref d1 k)))]
          [(eq? (dict-merge-delete-value) v) (dict-remove d0 k)]
          [else (dict-set d0 k v)])))

(module+ test
  (require rackunit)
  (check-equal?
   (dict-merge (hasheq 'foo "bar"
                       'bar "baz"
                       'request (hasheq 'delete-me "please"))
               (hasheq
                'bar 'DELETE
                'key "value"
                'request (hasheq 'version 1.1
                                 'delete-me 'DELETE
                                 'headers (hasheq 'Content-Type "foo"
                                                  'Content-Length 10))
                'response (hasheq 'headers (hasheq 'Content-Type "foo"
                                                   'Content-Length 10))))
   (hasheq
    'key "value"
    'request (hasheq 'version 1.1
                     'headers (hasheq 'Content-Length  10
                                      'Content-Type "foo"))
    'response (hasheq 'headers (hasheq 'Content-Length 10
                                       'Content-Type "foo"))
    'foo "bar")))
