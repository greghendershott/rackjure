#lang racket/base

(require version/utils
         racket/list
         racket/local
         kw-utils/arity+keywords)

(provide partial
         box-swap!)

(module+ test
  (require rackunit))

(define (kw-proc name arity+kws proc)
  (procedure-reduce-arity+keywords
   (procedure-rename (make-keyword-procedure proc) name)
   arity+kws))

(define (double-keyword-apply f kws-1 kw-args-1 kws-2 kw-args-2 rest-args)
  (keyword-apply keyword-apply kws-1 kw-args-1
                 f kws-2 kw-args-2
                 rest-args '()))

(define partial
  (kw-proc
   'partial (arity+keywords (arity-at-least 0) '() #f)
   (case-lambda
     [(kws-1 kw-args-1 f . args-1)
      (define f.arity+kws
        (procedure-arity+keywords f))
      (define arity+kws
        (arity+keywords-subtract f.arity+kws (length args-1) kws-1))
      (cond [(and (empty? kws-1) (empty? kw-args-1) (empty? args-1)) f]
            [(empty? (arity+keywords-arity arity+kws))
             (raise-too-many-partial-arguments-error f kws-1 kw-args-1 args-1)]
            [else
             (kw-proc
              'partial-f arity+kws
              (lambda (kws-2 kw-args-2 . args-2)
                (double-keyword-apply f kws-1 kw-args-1 kws-2 kw-args-2
                                      (append args-1 args-2))))])]
     [(kws-1 kw-args-1)
      (keyword-apply partial kws-1 kw-args-1 app '())])))

(define (raise-too-many-partial-arguments-error f kws-1 kw-args-1 args-1)
  (error 'partial
         (string-append "too many arguments" "\n"
                        "  function: ~v" "\n"
                        "  partial arguments: ~a")
         f
         (kw-args->string kws-1 kw-args-1 args-1)))

(define (kw-args->string kws kw-args rest-args)
  (define (string-append* . args)
    (apply string-append (flatten args)))
  (string-append*
   (for/list ([arg (in-list rest-args)])
     (format "~v " arg))
   (for/list ([kw (in-list kws)]
              [kw-arg (in-list kw-args)])
     (format "~a ~v " kw kw-arg))))


(define app
  (kw-proc
   'app (arity+keywords (arity-at-least 1) '() #f)
   (lambda (kws kw-args f . args)
     (keyword-apply f kws kw-args args))))



(module+ test
  ;; If we tested against the variable-arity `+` there would
  ;; be no difference between `partial` and `curry`.
  (define (+* x y) (+ x y))

  (check-equal? ((partial +*) 1 2) 3)
  (check-equal? ((partial +* 1) 2) 3)
  (check-equal? ((partial +* 1 2)) 3)
  (check-equal? ((partial) +* 1 2) 3)
  (check-exn (regexp (regexp-quote "too many arguments"))
             (λ () (partial +* 1 2 3)))
  
  ;; arity
  (check-equal? (procedure-arity+keywords (partial +*)) (arity+keywords 2 '() '()))
  (check-equal? (procedure-arity+keywords (partial +* 1)) (arity+keywords 1 '() '()))
  (check-equal? (procedure-arity+keywords (partial +* 1 2)) (arity+keywords 0 '() '()))
  
  ;; keywords
  (test-case "partial with keywords"
    (define (KE #:m m #:v v)
      (* 1/2 m v v))
    (check-equal? ((partial KE) #:m 2 #:v 1) 1)
    (check-equal? ((partial KE #:m 2) #:v 1) 1)
    (check-equal? ((partial KE #:m 2 #:v 1)) 1)
    (check-equal? ((partial) KE #:m 2 #:v 1) 1)
    (check-equal? ((partial #:m 2) KE #:v 1) 1)
    (check-exn (regexp (regexp-quote "too many arguments"))
               (λ () (partial KE #:whatever "idontkare")))
    ;; arity
    (check-equal? (procedure-arity+keywords (partial KE)) (arity+keywords 0 '(#:m #:v) '(#:m #:v)))
    (check-equal? (procedure-arity+keywords (partial KE #:m 2)) (arity+keywords 0 '(#:v) '(#:v)))
    (check-equal? (procedure-arity+keywords (partial KE #:v 1)) (arity+keywords 0 '(#:m) '(#:m)))
    (check-equal? (procedure-arity+keywords (partial KE #:m 2 #:v 1)) (arity+keywords 0 '() '()))
    (check-equal? (procedure-arity+keywords (partial)) (arity+keywords (arity-at-least 1) '() #f))
    (check-equal? (procedure-arity+keywords (partial #:m 2))(arity+keywords(arity-at-least 1)'()#f))))

(define (-box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))

(define (racket-5.92+?)
  (version<=? "5.92" (version)))

(define box-swap!
  (cond [(racket-5.92+?) -box-swap!]
        [(lambda _ (error 'box-swap! "requires Racket 5.92 or newer"))]))

(module+ test
  (require racket/future)
  (cond [(racket-5.92+?)
         (define shared (box 0))
         (define n-iterations 10000000)
         (define n-futures 10)

         (define (futures)
           (define (thunk)
             (for ([_ (in-range n-iterations)])
               ;; Use `+ 1` instead of `add1` to exercise `box-swap!`
               (box-swap! shared + 1)))

           (for/list ([_ n-futures])
             (future thunk)))

         (for ([f (futures)])
           (touch f))
         (check-equal? (unbox shared) (* n-iterations n-futures))]
        [else
         (check-exn exn:fail? (lambda () (box-swap! (box 0) + 1)))]))
