#lang racket/base

(require racket/contract
         racket/dict
         racket/function
         racket/match
         "alist.rkt")

(provide
 (contract-out
  [dict-merge (dict? dict? . -> . dict?)]
  [dict-merge-delete-value (parameter/c any/c)]
  [dict->curly-string (dict? . -> . string?)]))

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

;; Return a {} style string describing the nested dicts
(define (dict->curly-string d)
  (define (~v v) (format "~v" v))
  (let loop ([d d]
             [depth 0]
             [indent 0])
    (string-append
     "{"
     (for/fold ([s ""])
               ([(k v) (in-dict d)]
                [i (in-naturals)])
       (string-append
        s
        (cond [(zero? i) ""]
              [else (make-string (+ indent depth 1) #\space)])
        (~v k)
        " "
        (cond [(dict? v) (loop v
                               (add1 depth)
                               (+ 1 indent (string-length (~v k))))]
              [else (~v v)])
        (cond [(= i (- (length (dict-keys d)) 1)) "}"]
              [else "\n"]))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure style for dictionaries

(define get
  ;; Returns the value in a dictionary. If not found, returns the fail
  ;; argument if provided, else raises an exception like dict-ref did.
  ;; (Note that Clojure would return nil instead.)
  (case-lambda
    [(d k) (dict-ref d k)]
    [(d k fail) (dict-ref d k fail)]))

(module+ test
  (check-equal? (get '([a . 0][b . 1]) 'b) 1)
  (check-equal? (get '([a . 0][b . 1]) 'x 'z) 'z))

(define get-in
  ;; Returns the value in a nested dictionary. When not found, behaves
  ;; like get.
  (case-lambda
    [(d ks) (for/fold ([d d])
                      ([k (in-list ks)])
              (dict-ref d k))]
    [(d ks fail) (for/fold ([d d])
                           ([k (in-list ks)])
                   (dict-ref d k fail))]))

(module+ test
  (check-equal? (get-in '([a . ([b . ([c . 42])])]) '(a b c))
                42)
  (check-equal? (get-in '([a . ([b . ([c . 42])])]) '(a b x) 'z)
                'z))

(define (assoc d k v)
  (dict-set d k v))

(define (dissoc d k)
  (dict-remove d k))

(define (assoc-in d ks v)
  ;; Adds a key/value in a nested dictionary, where ks is a list of
  ;; keys and v is the new value, and returns a new nested dict.
  ;; If any levels do not exist, nested hashes will be created.
  ;;
  ;; WARNING: Supports only key/val dictionaries like hash-tables and
  ;; association lists -- NOT lists or vectors.
  ;; Why? Racket's dict-set does not work on lists or vectors.
  ;; Why? There are no functional list-set or vector-set functions.
  ;; Why? You really want a trie-backed structure to do those.
  (match ks
    [(list)      d]
    [(list k)    (assoc d k v)]
    [(cons k ks) (assoc d k (assoc-in (get d k (hash)) ks v))]))

(module+ test
  (let ([users (hash 'a (hash 'name "James" 'age 26)
                     'b (hash 'name "John" 'age 43))])
    (check-equal? (assoc-in users '(b age) 44)
                  (hash 'a (hash 'name "James" 'age 26)
                        'b (hash 'name "John" 'age 44)))
    (check-equal? (assoc-in users '(b password) "nhoJ")
                  (hash 'a (hash 'name "James" 'age 26)
                        'b (hash 'password "nhoJ" 'name "John" 'age 43)))
    ;; Also (assoc m 2 {...}) or (conj m {...})
    (check-equal? (assoc-in users '(c) (hash 'name "Jack" 'age 19))
                  (hash 'a (hash 'name "James" 'age 26)
                        'b (hash 'name "John" 'age 43)
                        'c (hash 'name "Jack" 'age 19)))))

(define (update-in d ks f . args)
  ;; 'Updates' a value in a nested dictionary, where ks is a list of
  ;; keys and f is a function that will take the old value and any
  ;; supplied args and return the new value, and returns a new nested
  ;; dict. If any levels do not exist, hash-tables will be created.
  ;;
  ;; WARNING: Supports only key/val dictionaries like hash-tables and
  ;; association lists -- NOT lists or vectors.
  ;; Why? Racket's dict-set does not work on lists or vectors.
  ;; Why? There are no functional list-set or vector-set functions.
  ;; Why? You really want a trie-backed structure to do those.
  (match ks
    [(list)      d]
    [(list k)    (assoc d k (apply f (get d k) args))]
    [(cons k ks) (assoc d k (apply update-in (get d k) ks f args))]))

(module+ test
  ;; This is like the Clojure example except the outer collection is a
  ;; hash not a vector (because our update-in doesn't work on vectors).
  (let ([users (hash 'a (hash 'name "James" 'age 26)
                     'b (hash 'name "John" 'age 43))])
    (check-equal? (update-in users '(b age) add1)
                  (hash 'a (hash 'name "James" 'age 26)
                        'b (hash 'name "John" 'age 44))))
  (check-equal? (update-in (hash 'a 3) '(a) / 4 5)
                (hash 'a 3/20)))
