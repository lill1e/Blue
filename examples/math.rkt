#lang racket
(require "../generator/gen.rkt")
(generate math "./math.blue")
(require racket/pretty)

;(pretty-print (math-expr-parse "1 * (2 + 5) + 3 * 4"))

(define (exec node)
 (match node
   [(? math-prim?) #:when (equal? (math-prim-variant node) 'left)
    (string->number (math-prim-val node))]
   [(? math-prim?) #:when (equal? (math-prim-variant node) 'right)
    (exec (math-prim-val node))]
   [(? math-expr?) (exec (math-expr-body node))]
   [(? math-group?) (exec (math-group-body node))]
   [(? math-add?) 
    (match (math-add-op node)
        [(math-add_op _ "+") (+ (exec (math-add-lhs node)) (exec (math-add-rhs node)))]
        [(math-add_op _ "-") (- (exec (math-add-lhs node)) (exec (math-add-rhs node)))]
        [_ (exec (math-add-lhs node))])]
   [(? math-mul?) 
    (match (math-mul-op node)
        [(math-mul_op _ "*") (* (exec (math-mul-lhs node)) (exec (math-mul-rhs node)))]
        [(math-mul_op _ "/") (/ (exec (math-mul-lhs node)) (exec (math-mul-rhs node)))]
        [_ (exec (math-mul-lhs node))])]))

(define (run expr)
    (pretty-print (math-expr-parse expr))
    (exec (math-expr-parse expr)))

(run "1 * (3 + 2) * 9 + 10")