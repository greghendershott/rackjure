#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     racket/local)
         (only-in "conditionals.rkt" if-let)
         racket/local)

(provide ~>
         ~>>
         ~>%
         ~>_
         some~>
         some~>>
         some~>%
         some~>_
         %
         )

(module+ test
  (require "check-expansion.rkt")
  (define-namespace-anchor anchor))

;; Clojure threading macros. Original versions courtesy of Asumu
;; Takikawa.
;;
;; Rewritten with help from Sam Tobin-Hochstadt to use whatever #%app
;; is bound in the lexical context of the macro usage. That way, these
;; will expand to the default Racket #%app, or to the applicative
;; dictionary #%app from app.rkt that's part of #lang rackjure, or to
;; whatever other #%app is bound at the macro usage site. This is
;; handled by the keep-stxctx combinator.
;;
;; Among other things this allows using `(require rackjure/threading)`
;; to get ~> and ~>> without the applicative dict overhead associated
;; with #lang rackjure.
;;
;; Also rewritten to use a fold instead of recursive invocations to
;; avoid the issues described in CLJ-1121.
;;
;; Also including some forms based on git://github.com/rplevy/swiss-arrows

(begin-for-syntax 
  (define stx-coerce-to-list
    (syntax-parser
     [((~literal quote) x) #'((quote x))]
     ;; ^ We want to end up with ((quote x) y), NOT (quote x y).
     [(form ...) #'(form ...)]
     [ form      #'(form)]))

  (define ((keep-stxctx f) stx . args)
    (datum->syntax stx (syntax-e (apply f stx args)) stx stx))

  (define (threading-syntax-parser threader)
    (syntax-parser
     [(_ first rest ...)
      (define normalized-rest (stx-map (keep-stxctx stx-coerce-to-list)
                                       #'(rest ...)))
      (foldl (keep-stxctx threader) #'first normalized-rest)])))

(module+ test
  (define-syntax test-threader
    (threading-syntax-parser
     (lambda (form nested-form) #`(#,form . #,nested-form))))
   
  (check-expand-once anchor #'(test-threader 1) #'1)
  (check-expand-once anchor #'(test-threader 1 f (g 2)) #'((g 2) (f) . 1)))

(define-syntax ~>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f #,nested-form r ...)]))))

(define-syntax ~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     (syntax-parse form [(f r ...) #`(f r ... #,nested-form)]))))

(define-syntax some~>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~> tmp #,form) #f))))

(define-syntax some~>>
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~>> tmp #,form) #f))))

(define-syntax %
  (lambda (stx)
    (raise-syntax-error #f "must be used in either a #λ, #fn, #lambda, or ~>% form" stx)))

(begin-for-syntax
  (define (insertion-threading-syntax-parser placeholder?)
    (local [(define (threader form nested-form)
              (define inc (includes-placeholder? form nested-form))
              (cond [inc inc]
                    [else #`(#,form #,nested-form)]))
            (define (includes-placeholder? form nested-form)
              (syntax-parse form
                [form:id (cond [(placeholder? #'form) nested-form]
                               [else #f])]
                [form #:when (syntax-property #'form 'afl) #f]
                [() #f]
                [(sub-fst . sub-rst)
                 (let ([sub-fst-inc (includes-placeholder? #'sub-fst nested-form)]
                       [sub-rst-inc (includes-placeholder? #'sub-rst nested-form)])
                   (cond [(and sub-fst-inc sub-rst-inc) #`(#,sub-fst-inc . #,sub-rst-inc)]
                         [sub-fst-inc #`(#,sub-fst-inc . sub-rst)]
                         [sub-rst-inc #`(sub-fst . #,sub-rst-inc)]
                         [else #f]))]
                [#(sub ...)
                 (let ([lst-inc (includes-placeholder? #'(sub ...) nested-form)])
                   (cond [lst-inc (syntax->datum form (list->vector (syntax->list lst-inc)) form form)]
                         [else #f]))]
                [_ #f]))
            (define (threading-syntax-parser threader)
              (syntax-parser
                [(_ first rest ...)
                 (define normalized-rest (syntax->list #'(rest ...)))
                 (foldl (keep-stxctx threader) #'first normalized-rest)]))]
      (threading-syntax-parser threader)))
  )

(define-syntax ~>%
  (insertion-threading-syntax-parser
   (lambda (stx)
     (syntax-parse stx
       [(~literal %) #t]
       [_ #f]))))

(define-syntax ~>_
  (insertion-threading-syntax-parser
   (lambda (stx)
     (syntax-parse stx
       [(~literal _) #t]
       [_ #f]))))

(define-syntax some~>%
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~>% tmp #,form) #f))))

(define-syntax some~>_
  (threading-syntax-parser
   (lambda (form nested-form)
     #`(if-let [tmp #,nested-form] (~>_ tmp #,form) #f))))

(define-syntax ~>f
  (local [(define (threader form nested-form)
            #`(#,form #,nested-form))
          (define (threading-syntax-parser threader)
            (syntax-parser
              [(_ first rest ...)
               (define normalized-rest (syntax->list #'(rest ...)))
               (foldl (keep-stxctx threader) #'first normalized-rest)]))
          (define parse (threading-syntax-parser threader))]
    (lambda (stx)
      (syntax-parse stx
        [(_ . stuff) (parse stx)]
        [_:id #'~>f-function]
        ))))

(define ~>f-function
  (local [(define (~>f expr . fs)
            (~>f--lst expr fs))
          (define (~>f--lst expr fs)
            (cond [(empty? fs) expr]
                  [else (define f (first fs))
                        (~>f--lst (f expr) (rest fs))]))
          (define empty? null?)
          (define first car)
          (define rest cdr)]
    ~>f))

(define-syntax some~>f
  (local [(define (threader form nested-form)
            #`(if-let [tmp #,nested-form] (#,form tmp) #f))
          (define (threading-syntax-parser threader)
            (syntax-parser
              [(_ first rest ...)
               (define normalized-rest (syntax->list #'(rest ...)))
               (foldl (keep-stxctx threader) #'first normalized-rest)]))
          (define parse (threading-syntax-parser threader))]
    (lambda (stx)
      (syntax-parse stx
        [(_ . stuff) (parse stx)]
        [_:id #'some~>f-function]
        ))))

(define some~>f-function
  (local [(define (some~>f expr . fs)
            (some~>f--lst expr fs))
          (define (some~>f--lst expr fs)
            (cond [(empty? fs) expr]
                  [(false? expr) #f]
                  [else (define f (first fs))
                        (some~>f--lst (f expr) (rest fs))]))
          (define empty? null?)
          (define first car)
          (define rest cdr)
          (define false? not)]
    some~>f))


(module+ test
  (check-expand-once anchor #'(~> 1 (f 2))     #'(f 1 2))
  (check-expand-once anchor #'(~> #t (if 1 2)) #'(if #t 1 2))
  ;; ^ Check that it works with syntax forms
  
  (check-expand-once  anchor #'(~>> 1 (f 2))       #'(f 2 1))
  (check-expand-fully anchor #'(~>> 1 + (~>> 1 +)) #'(#%app + '1 (#%app + '1)))
  ;; ^ Example from CLJ-1121
  
  (check-expand-once anchor
                     #'(some~> 1 f)
                     #'(if-let [tmp 1](~> tmp (f)) #f))
  
  (check-expand-once anchor
                     #'(some~>> 1 f)
                     #'(if-let [tmp 1](~>> tmp (f)) #f))
  
  (check-expand-once anchor #'(~>_ 1 (f _ 2))     #'(f 1 2))
  (check-expand-once anchor #'(~>_ #t (if _ 1 2)) #'(if #t 1 2))
  (check-expand-once anchor #'(~>_ 1 (f 2 _))     #'(f 2 1))
  (check-expand-fully anchor #'(~>_ 1 + (~>> 1 + _)) #'(#%app + '1 (#%app + '1)))
  (check-expand-once anchor #'(~>_ 1 _) #'1)
  (check-expand-once anchor
                     #'(~>_ #"foobar" bytes-length (number->string _ 16) string->bytes/utf-8)
                     #'(string->bytes/utf-8 (number->string (bytes-length #"foobar") 16)))
  (check-expand-once anchor
                     #`(~>% #"foobar"
                            bytes-length
                            #,(syntax-property #'(λ (%) (number->string % 16)) 'afl #t)
                            string->bytes/utf-8)
                     #'(string->bytes/utf-8 ((λ (%) (number->string % 16)) (bytes-length #"foobar"))))
  (check-expand-once anchor
                     #'(~>f #"foobar"
                            bytes-length
                            (λ (%) (number->string % 16))
                            string->bytes/utf-8)
                     #'(string->bytes/utf-8 ((λ (%) (number->string % 16)) (bytes-length #"foobar"))))
  (check-equal? (apply ~>f #"foobar"
                       (list bytes-length
                             (λ (%) (number->string % 16))
                             string->bytes/utf-8))
                (string->bytes/utf-8 ((λ (%) (number->string % 16)) (bytes-length #"foobar"))))
  )

;; Confirm expansion using default #%app
(module+ test
  (module test-plain-app racket/base
    (require (submod ".." "..")) ;; for ~>
    (require "check-expansion.rkt")
    (define-namespace-anchor anchor)
    ;; 1. Directly; expanding ~> macro
    (check-expand-fully anchor
                        #'(~> 1 +)
                        #'(#%app + (quote 1)))
    ;; 2. Indirectly; no implicit require of wrong #%app
    (check-expand-fully anchor
                        #'((hasheq 'a 42) 'a)
                        #'(#%app (#%app hasheq (quote a) (quote 42))
                                 (quote a))))
  (require 'test-plain-app))

;; Confirm expansion using our applicative dict #%app
(module+ test
  (module test-dict-app racket/base
    (require (submod ".." "..")) ;; for ~>
    (require (rename-in "app.rkt" [-#%app #%app]))
    (require "check-expansion.rkt")
    (define-namespace-anchor anchor)
    ;; 1. Directly; expanding ~> macro
    (check-expand-fully anchor
                        #'(~> 1 +)
                        #'(#%app maybe-dict-ref + (quote 1)))
    ;; 2. Indirectly; no implicit require of wrong #%app
    (check-expand-fully anchor
                        #'((hasheq 'a 42) 'a)
                        #'(#%app maybe-dict-ref
                                 (#%app maybe-dict-set
                                        hasheq (quote a) (quote 42)) (quote a))))
  (require 'test-dict-app))
