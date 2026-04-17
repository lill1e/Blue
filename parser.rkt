#lang racket

(require "parser/lexer.rkt")
(require "parser/parser.rkt")
(require "parser/passes.rkt")

(define passes (list (cons "Operator Simplification" ops->structs)))

(define passify
  (λ (p current-passes)
    (if (null? current-passes)
        p
        (let* [(current-pass (car current-passes))
               (pass-res ((cdr current-pass) p))]
          (passify pass-res (cdr current-passes))))))

(define file->blue
  (λ (file-name)
    (passify (parse (lex file-name)) passes)))

(provide file->blue)
