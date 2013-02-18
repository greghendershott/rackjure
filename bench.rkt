#lang racket/base

(module normal racket
  (displayln "#lang racket ----------")
  (define-syntax (time* stx)
    (syntax-case stx ()
      [(_ e)
       (with-syntax ([ed (format "~v" (syntax->datum #'e))])
         #`(begin
             (displayln ed)
             (for ([i 3]) (collect-garbage))
             (time e)
             (newline)))]))
  ;; Normal function application
  (time* (for ([i 10000000])
           (+ 1 1)))
  )

(module special rackjure/rackjure
  (displayln "#lang rackjure ----------")
  (define-syntax (time* stx)
    (syntax-case stx ()
      [(_ e)
       (with-syntax ([ed (format "~v" (syntax->datum #'e))])
         #`(begin
             (displayln ed)
             (for ([i 3]) (collect-garbage))
             (time e)
             (newline)))]))
  ;; Normal function application
  (time* (for ([i 10000000])
           (+ 1 1)))
  ;; Dict ref via various forms
  (let ([d {'a {'b {'c 0}}}])
    (time* (for ([i 100000])
             (dict-ref d 'a)))
    (time* (for ([i 100000])
             (d 'a)))
    (time* (for ([i 100000])
             ('a d))))

  (let ([d {'a {'b {'c 0}}}])
    (time* (for ([i 100000])
             (dict-ref (dict-ref (dict-ref d 'a) 'b) 'c)))
    (time* (for ([i 100000])
             (~> d 'a 'b 'c))))
  )

(require 'normal)
(require 'special)
