#lang racket/base

(require syntax/parse
         racket/match
         (only-in racket/port input-port-append)
         (only-in racket/list filter-map remove-duplicates append*)
         rackjure/threading
         (for-syntax racket/base))

(provide wrapper1
         lambda-readtable)

(define (parse stx)
  (with-syntax ([args (parse-args stx)]
                [%1 (datum->syntax stx '%1 stx)]
                [body stx])
    #'(lambda args
        (define-syntax % (make-rename-transformer #'%1))
        body)))

(module+ test
  (require rackunit)
  ;; These test `parse`. See test.rkt for tests of readtable use per se.
  (define chk (compose1 syntax->datum parse))
  (check-equal? (chk #'(+))
                '(lambda ()
                  (define-syntax % (make-rename-transformer #'%1))
                  (+)))
  (check-equal? (chk #'(+ 2 %1 %1))
                '(lambda (%1)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %1 %1)))
  (check-equal? (chk #'(+ 2 %3 %2 %1))
                '(lambda (%1 %2 %3)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %3 %2 %1)))
  (check-equal? (chk #'(apply list* % %&))
                '(lambda (%1 . %&)
                  (define-syntax % (make-rename-transformer #'%1))
                  (apply list* % %&))))

;; parse-args : Stx -> KW-Formals-Stx
(define (parse-args stx)
  ;; Filter the stxs to those that start with %, 
  ;; find the maximum, find whether there are any
  ;; keyword arguments or a rest argument, and
  ;; produce kw-formals based on that.
  (define-values (max-num rest? kws)
    (find-arg-info stx))
  (define datum-kw-formals
    (append (for/list ([n (in-range 1 (add1 max-num))])
              (string->symbol (string-append "%" (number->string n))))
            (append*
             (for/list ([kw (in-list kws)])
               (list kw (string->symbol (string-append "%#:" (keyword->string kw))))))
            (cond [rest? '%&]
                  [else '()])))
  (datum->syntax stx datum-kw-formals stx))

;; find-arg-info : Any -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info v)
  (match (maybe-syntax-e v)
    [(? symbol? sym) (find-arg-info/sym sym)]
    [(? pair? pair)  (find-arg-info/pair pair)]
    [_               (return)]))

;; find-arg-info/sym : Symbol -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/sym sym)
  (match (~> sym symbol->string string->list)
    [(list)           (return)]
    [(list  #\%)      (return #:max-num 1)]
    [(list  #\% #\&)  (return #:rest? #t)]
    [(list* #\% #\# #\: cs)
     (return #:kws (~> cs list->string string->keyword list))]
    [(list #\% (? char-numeric? cs) ...)
     (return #:max-num (~> cs list->string string->number))]
    [_ (return)]))

;; find-arg-info/pair :
;;   (Cons Symbol Symbol) -> (Values Natural Boolean (Listof Keyword))
(define (find-arg-info/pair pair)
  (define-values (car.max-num car.rest? car.kws)
    (find-arg-info (car pair)))
  (define-values (cdr.max-num cdr.rest? cdr.kws)
    (find-arg-info (cdr pair)))
  (return #:max-num (max car.max-num cdr.max-num)
          #:rest? (or car.rest? cdr.rest?)
          #:kws (remove-duplicates (append car.kws cdr.kws))))

(define (return #:max-num [max-num 0] #:rest? [rest? #f] #:kws [kws '()])
  (values max-num rest? kws))

(define (maybe-syntax-e stx)
  (cond [(syntax? stx) (syntax-e stx)]
        [else stx]))

(define ((make-reader-proc [orig-readtable (current-readtable)]) ch in src line col pos)
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

;(define orig-readtable (current-readtable))

(define (make-lambda-readtable [orig-readtable (current-readtable)])
  (define reader-proc (make-reader-proc orig-readtable))
  (~> orig-readtable
      (make-readtable #\λ 'dispatch-macro reader-proc)
      (make-readtable #\f 'dispatch-macro reader-proc)
      (make-readtable #\l 'dispatch-macro reader-proc)))

(define lambda-readtable (make-lambda-readtable))
;(define reader-proc (make-reader-proc))

;(current-readtable lambda-readtable)

;; A `#:wrapper1` for `syntax/module-reader`
(define (wrapper1 thk)
  (define orig-readtable (current-readtable))
  (parameterize ([current-readtable (make-lambda-readtable orig-readtable)])
    (thk)))
