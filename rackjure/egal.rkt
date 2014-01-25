#lang racket/base

;; all hail Baker! http://home.pipeline.com/~hbaker1/ObjectIdentity.html

(require racket/stream
         racket/set)

(provide egal?)

(define (egal? x y)
  (cond [(and (boolean? x) (boolean? y)) (eq? x y)]
        [(and (number? x) (number? y)) (eqv? x y)]
        [(and (char? x) (char? y)) (char=? x y)]
        [(and (symbol? x) (symbol? y)) (eq? x y)]
        [(and (box? x) (box? y)) (eq? x y)]
        [(and (regexp? x) (regexp? y)) (equal? x y)]
        [(and (sequence? x) (sequence? y)) (egal-sequence? x y)]
        [(and (procedure? x) (procedure? y)) (egal-procedure? x y)]
        [(and (void? x) (void? y) #t)]
        [(and (pair? x) (pair? y)) (equal? x y)]
        [else (eq? x y)]))

(define (egal-procedure? x y)
  ;; As far as I know, source/environment isn't available for procedures
  #f)

(define (egal-sequence? x y)
  (if (and (immutable? x) (immutable? y))
      (cond [(and (string? x) (string? y)) (equal? x y)]
            [(and (bytes? x) (bytes? y)) (equal? x y)]
            [(and (vector? x) (vector? y)) (equal? x y)]
            [(and (hash? x) (hash? y)) (equal? x y)]
            [else (eq? x y)])
      ;; the immutable? predicate is undefined on most types
      (cond [(and (stream? x) (stream? y)) (egal-elements? x y)]
            [(and (set? x) (set? y)) (egal-elements? x y)]
            [else (eq? x y)])))

(define (egal-elements? x y)
  (or (and (stream-empty? x) (stream-empty? y))
      (and (egal? (stream-first x) (stream-first y))
           (egal-elements? (stream-rest x) (stream-rest y)))))

(module* test racket
  (require (submod "..")
           rackunit)
  (define-syntax (== stx)
    (syntax-case stx ()
      [(_ x y) (syntax/loc stx
                 (check-true (egal? x y)))]))
  (define-syntax (!= stx)
    (syntax-case stx ()
      [(_ x y) (syntax/loc stx
                 (check-false (egal? x y)))]))
  ;; boolean?
  (== #t #t)
  (!= #t #f)
  ;; number?
  (== 0 0)
  (== 0.0 0.0)
  (== 1/2 1/2)
  (!= 0 1)
  (!= 0.0 1.0)
  (!= 1/2 2/1)
  ;; char?
  (== #\a #\a)
  (!= #\a #\b)
  ;; symbol?
  (== 'a 'a)
  (!= 'a 'b)
  ;; box?
  (let ([a (box 'a)]
        [b (box 'b)])
    (== a a)
    (!= a b))
  ;; regexp?
  (== #rx"a" #rx"a")
  (!= #rx"a" #rx"b")
  (== #px"a" #px"a")
  (!= #px"a" #px"b")
  ;; void?
  (== (void) (void))
  ;; Note that `pair`s are _not_ sequence?
  (== (cons 0 0) (cons 0 0))
  (!= (cons 0 0) (cons 1 1))

  ;;
  ;; sequences
  ;;

  ;; Although "string" literals are immutable, `string` isn't
  (== "a" "a")
  (!= "a" "b")
  (!= (string #\a) (string #\a))
  (== (string->immutable-string (string #\a))
      (string->immutable-string (string #\a)))

  ;; Although #"bytes" literals are immutable, `bytes` isn't
  (== #"a" #"a")
  (!= #"a" #"b")
  (!= (bytes 0) (bytes 0))
  (== (bytes->immutable-bytes (bytes 0))
      (bytes->immutable-bytes (bytes 0)))

  ;; Although #(0) literals are immutable (as is obviously
  ;; `vector-immutable`), `vector` isn't.
  (== #(0) #(0))
  (!= #(0) #(1))
  (!= (vector 0) (vector 0))
  (== (vector-immutable 0) (vector-immutable 0))

  ;; immutable hash variants...
  (== (hash 0 0) (hash 0 0))
  (!= (hash 0 0) (hash 0 1))
  (== (hasheq '0 0) (hasheq '0 0))
  (!= (hasheq '0 0) (hasheq '0 1))
  (== (make-immutable-hash '([0 0])) (make-immutable-hash '([0 0])))
  (!= (make-immutable-hash '([0 0])) (make-immutable-hash '([0 1])))
  (== (make-immutable-hasheq '([k 0])) (make-immutable-hasheq '([k 0])))
  (!= (make-immutable-hasheq '([k 0])) (make-immutable-hasheq '([k 1])))
  ;; mutable hash variants...
  (!= (make-hash '([0 0])) (make-hash '([0 0])))
  (!= (make-hasheq '([k 0])) (make-hash '([k 0])))

  ;; stream? is true of many things we test here, but just use `list`
  (== (list 0 0) (list 0 0))
  (!= (list 0 0) (list 1 1))

  (== (set 0) (set 0))
  (!= (set 0) (set 1)))
