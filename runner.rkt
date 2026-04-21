#lang racket

(require "parser.rkt")
(require "type-check.rkt")

(define run
  (λ (file-name)
    (let [(obj (file->blue file-name))]
      (displayln "AST Representation:")
      (pretty-print obj)
      (displayln (if (type-check obj) "Well Formed BNF" "There is an issue in the provided BNF, please review the grammar (README.md)")))))

(run "example.blue")
