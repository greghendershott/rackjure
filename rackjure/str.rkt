#lang racket/base

(require racket/contract racket/string)

(provide
 (contract-out
  [str (()
        (#:fmt (any/c . -> . string?) #:sep string?)
        #:rest list?
        . ->* .
        string?)]))
    
;; Don't require racket/format just for this. Define it here so can
;; work with older versions of Racket, too.
(define (~a s)
  (format "~a" s))

;; #:fmt: The function to apply. Defaults to ~a. Could be ~v, or
;; anything that returns a string?
;;
;; #:sep: A string to add between each. Defaults to "".
(define (str #:fmt [fmt ~a] #:sep [sep ""] . xs)
  (string-join (map fmt xs) sep))

(module* test racket ;; To test module-boundary contracts, must use
                     ;; module* and (require (submod "..")).
  (require (submod "..") rackunit)
  (check-equal? (str) "")
  (check-equal? (str "hi") "hi")
  (check-equal? (str 1) "1")
  (check-equal? (str #f) "#f")
  (check-equal? (str "Yo" "Yo") "YoYo")
  (check-equal? (str "Yo" "Yo" "Ma") "YoYoMa")
  (check-equal? (str #:fmt ~v "Yo" "Yo") "\"Yo\"\"Yo\"")
  (check-equal? (str #:sep " " "Yo" "Yo") "Yo Yo")
  (check-equal? (str #:fmt ~v  #:sep " " "Yo" "Yo") "\"Yo\" \"Yo\"")
  (check-equal? (str '(0 1 2 3 4 5 6 7 8 9)) "(0 1 2 3 4 5 6 7 8 9)")
  (check-equal? (apply str '(0 1 2 3 4 5 6 7 8 9)) "0123456789")
  (check-exn exn:fail:contract?
             (thunk (str #:fmt values 1))) ;; not (any/c -> string?)
  (check-exn exn:fail:contract?
             (thunk (str #:sep #f 1)))) ;; not string?
