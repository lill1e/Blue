#lang racket

(require "parser/structs.rkt")

(define regex?
  (λ (regex-str)
    (with-handlers [(exn:fail:contract? (λ (_) #f))] (begin (regexp regex-str) #t))))

(define field-rhs?
  (λ (field-rhs)
    (match field-rhs
      [(? list?) (andmap field-rhs? field-rhs)]
      [(or (? Identifier?)
           (? String?)) #t]
      [(As lhs rhs) (and (Identifier? lhs) (Identifier? rhs))]
      [(Or lhs rhs) (and (field-rhs? lhs) (field-rhs? rhs))])))

(define is-field?
  (λ (grammar-field)
    (and (or (Field? grammar-field)
             (TokenField? grammar-field))
         (match grammar-field
           [(Field lhs rhs) (and (Identifier? lhs) (field-rhs? rhs))]
           [(TokenField lhs rhs) (and (Identifier? lhs) (String? rhs) (regex? (String-s rhs)))]))))

(define type-check
  (λ (obj) (andmap is-field? obj)))

(provide type-check)
