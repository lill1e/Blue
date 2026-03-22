#lang racket

(require "parser.rkt")

;; TODO: what is our interpreter going to do?
;;       - maybe generate tentative parsers
(define interpret
  (λ (file-name)
    (file->blue file-name)))

;; TODO: make an example parser for the example BNF (Binary Numbers)
;;       - the lexer can be ripped from this project
;;       - similarly, the parser follows the Blue BNF using recursive descent

(interpret "example.blue")
