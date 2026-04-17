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
         (? String?)) e]
      [(Binary (Symbol '\|) lhs rhs) (Or (if (list? lhs)
                                             (ops->structs lhs)
                                             (map ops->structs lhs))
                                         (if (list? rhs)
                                             (ops->structs rhs)
                                             (map ops->structs rhs)))]
      [(Field lhs rhs) (Field lhs (ops->structs rhs))]
      [(TokenField lhs rhs) (TokenField lhs (ops->structs rhs))])))

(provide ops->structs)
