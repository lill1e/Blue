#lang racket
(require "../generator/gen.rkt")
(generate math "./math.blue")
(require racket/pretty)

(pretty-print (math-expr-parse "1 * 2 + 3 * 4"))