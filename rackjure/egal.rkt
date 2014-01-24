#lang racket

;; all hail Baker! http://home.pipeline.com/~hbaker1/ObjectIdentity.html

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
            [(and (pair? x) (pair? y)) (egal-elements? x y)]
            [(and (set? x) (set? y)) (egal-elements? x y)]
            [else (eq? x y)])))

(define (egal-elements? x y)
  (or (and (stream-empty? x) (stream-empty? y))
      (and (egal? (stream-first x) (stream-first y))
           (egal-elements? (stream-rest x) (stream-rest y)))))
