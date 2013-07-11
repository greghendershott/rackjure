#lang racket/base

(require racket/contract racket/dict racket/function "alist.rkt")

(provide
 (contract-out
  [dict-merge (dict? dict? . -> . dict?)]
  [dict-merge-delete-value (() (any/c) . ->* . any/c)]
  [dict->curly-string ((dict?)
                       (exact-nonnegative-integer? exact-nonnegative-integer?)
                       . ->* . string?)]
  ))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a {} style string describing the nestead dicts
(define (dict->curly-string d [depth 0] [indent 0])
  (define concat string-append)         ;minimize indent
  (define ~v (curry format "~v"))
  (concat "{"
          (for/fold ([s ""])
                    ([(k v) (in-dict d)]
                     [i (in-naturals)])
            (concat s
                    (cond [(zero? i) ""]
                          [else (make-string (+ indent depth 1) #\space)])
                    (~v k)
                    " "
                    (cond [(dict? v) (dict->curly-string
                                      v
                                      (add1 depth)
                                      (+ 1 indent (string-length (~v k))))]
                          [else (~v v)])
                    (cond [(= i (- (length (dict-keys d)) 1)) "}"]
                          [else "\n"])
                    ))))

(module+ test
  (check-equal?
   (dict->curly-string
    '([a . 0]
      [b . 0]
      [c . ([a . 0]
            [b . 0]
            [c . ([a . 0]
                  [b . 0]
                  [c . 0])])]))
   #<<EOF
{'a 0
 'b 0
 'c {'a 0
     'b 0
     'c {'a 0
         'b 0
         'c 0}}}
EOF
))
