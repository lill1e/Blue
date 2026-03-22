#lang racket

(require "structs.rkt")

;; make-binary : Tokens ASTNode (Tokens -> ASTNode) Symbols -> ASTNode
(define make-binary
  (λ (tokens lhs next syms)
    (match tokens
      [(? null?) (error "reached end of tokens without a match")]
      [`(,(Symbol s) . ,other-tokens) #:when (memv s syms)
                                      (match/values
                                       (next other-tokens null)
                                       [(rhs post-tokens) (make-binary post-tokens (Binary (car tokens) lhs rhs) next syms)])]
      [_ (values lhs tokens)])))

(define list-if-needed
  (λ (v) (if (list? v) v (list v))))

(provide make-binary list-if-needed)
