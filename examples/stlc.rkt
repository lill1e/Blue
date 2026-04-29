#lang racket
(require "../blue.rkt")
(generate stlc "./stlc.blue")
(define program (stlc-program-parse "((λ x x) 42)"))
(stlc-program-expr program)
