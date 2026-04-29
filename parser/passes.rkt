#lang racket

(require "structs.rkt")

(define ops->structs
  (λ (e)
    (match e
      [(? list?) (map ops->structs e)]
      [(or
         (? Identifier?)
         (? Symbol?)
         (? Newline?)
         (? String?)
         (? As?)) e]
      [(Binary (Symbol '\|) lhs rhs) (Or (if (list? lhs)
                                             (map ops->structs lhs)
                                             (ops->structs lhs))
                                         (if (list? rhs)
                                             (map ops->structs rhs)
                                             (ops->structs rhs)))]
      [(Field lhs rhs) (Field lhs (ops->structs rhs))]
      [(TokenField lhs rhs) (TokenField lhs (ops->structs rhs))])))

(provide ops->structs)
