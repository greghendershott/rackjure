#lang racket/base

(require syntax/parse
         racket/match
         (only-in racket/port input-port-append)
         (only-in racket/list filter-map remove-duplicates)
         rackjure/threading)

(provide wrapper1
         lambda-readtable)

(define (%n-args stxs)
  ;; Filter the stxs to those that are %1..%9 or %, removing
  ;; duplicates and sorting textually.
  ;;
  ;; Although Clojure doc implies it supports %10 and greater, this
  ;; doesn't. IMHO using that in a reader lambda is a code smell.
  ;;
  ;; Caveat: This does no checking for non-contiguous numbers (such as
  ;; %1 and %3 but missing %2) or for using both % and %1.
  (define (symbol< a b)
    (string<? (symbol->string a) (symbol->string b)))
  (~> (filter-map (lambda (stx)
                    (define e (syntax-e stx))
                    (and (symbol? e)
                         (match (symbol->string e)
                           [(pregexp "^%[1-9]$") stx]
                           [(pregexp "^%$")      stx]
                           [_ #f])))
                  stxs)
      (remove-duplicates #:key syntax-e)
      (sort symbol< #:key syntax-e))) ;; textual sort fine for %1 .. %9

(define (%&-arg? stxs)
  (for/or ([stx stxs])
    (eq? '%& (syntax-e stx))))

(define (parse stx)
  (syntax-parse stx
    [(e:expr ...)
     (with-syntax ([(args ...) (%n-args (syntax->list #'(e ...)))])
       (cond [(%&-arg? (syntax->list #'(e ...)))
              #'(lambda (args ... . %&)
                  (e ...))]
             [else
              #'(lambda (args ...)
                  (e ...))]))]))

(module+ test
  (require rackunit)
  ;; These test `parse`. See test.rkt for tests of readtable use per se.
  (define chk (compose1 syntax->datum parse))
  (check-equal? (chk '(+ 2 %1 %1))
                '(lambda (%1) (+ 2 %1 %1)))
  (check-equal? (chk '(+ 2 %3 %2 %1))
                '(lambda (%1 %2 %3) (+ 2 %3 %2 %1)))
  (check-equal? (chk '(apply list* % %&))
                '(lambda (% . %&) (apply list* % %&))))

(define (reader-proc ch in src line col pos)
  (define (normal-read-syntax src in)
    (parameterize ([current-readtable orig-readtable])
      (read-syntax src in)))
  (define (unget-normal-read-syntax str src in)
    (normal-read-syntax src (input-port-append #f (open-input-string str) in)))
  (define (peek/read? str in)
    (and (equal? str (peek-string (string-length str) 0 in))
         (read-string (string-length str) in)))
  (cond [(eq? ch #\l)
         (cond [(peek/read? "ambda" in) (~> (normal-read-syntax src in) parse)]
               [else (unget-normal-read-syntax "#l" src in)])]
        [(eq? ch #\f)
         (cond [(peek/read? "n" in) (~> (normal-read-syntax src in) parse)]
               [else (unget-normal-read-syntax "#f" src in)])]
        [else (~> (normal-read-syntax src in) parse)])) ;single letter e.g. #λ

(define orig-readtable (current-readtable))

(define lambda-readtable (~> orig-readtable
                             (make-readtable #\λ 'dispatch-macro reader-proc)
                             (make-readtable #\f 'dispatch-macro reader-proc)
                             (make-readtable #\l 'dispatch-macro reader-proc)))

(current-readtable lambda-readtable)

;; A `#:wrapper1` for `syntax/module-reader`
(define (wrapper1 thk)
  (parameterize ([current-readtable lambda-readtable])
    (thk)))
