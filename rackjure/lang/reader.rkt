#lang s-exp syntax/module-reader
rackjure
#:wrapper1 wrapper1
#:language-info #(rackjure/lang/language-info get-language-info #f)

(require "../lambda-reader.rkt")
