#lang racket/base

(require racket/contract racket/string racket/function)

(provide
 (contract-out
  [str (()
        (#:fmt (any/c . -> . string?) #:sep string?)
        #:rest list?
        . ->* .
        (and/c immutable? string?))]))

;; #:fmt: The function to apply. Defaults to ~a. Could be ~v, or
;; anything that returns a string?
;;
;; #:sep: A string to add between each. Defaults to "".
(define (str #:fmt [fmt (curry format "~a")] #:sep [sep ""] . xs)
  (string->immutable-string (string-join (map fmt xs) sep)))

(module* test racket/base
  (require (submod "..")
           rackunit
           (only-in racket/function curry thunk))
  (check-equal? (str) "")
  (check-equal? (str "hi") "hi")
  (check-equal? (str 1) "1")
  (check-equal? (str #f) "#f")
  (check-equal? (str "Yo" "Yo") "YoYo")
  (check-equal? (str "Yo" "Yo" "Ma") "YoYoMa")
  (check-equal? (str #:fmt (curry format "~v") "Yo" "Yo") "\"Yo\"\"Yo\"")
  (check-equal? (str #:sep " " "Yo" "Yo") "Yo Yo")
  (check-equal? (str #:fmt (curry format "~v") #:sep " " "Yo" "Yo") "\"Yo\" \"Yo\"")
  (check-equal? (str '(0 1 2 3 4 5 6 7 8 9)) "(0 1 2 3 4 5 6 7 8 9)")
  (check-equal? (apply str '(0 1 2 3 4 5 6 7 8 9)) "0123456789")
  (check-exn exn:fail:contract?
             (thunk (str #:fmt values 1))) ;; not (any/c -> string?)
  (check-exn exn:fail:contract?
             (thunk (str #:sep #f 1)))) ;; not string?
