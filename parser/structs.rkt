#lang racket

(define-struct Identifier (ident) #:transparent)
(define-struct Symbol (sym) #:transparent)
(define-struct Newline () #:transparent)
(define-struct String (s) #:transparent)

(define-struct Field (lhs rhs) #:transparent)
(define-struct Binary (op lhs rhs) #:transparent)
(define-struct Or (lhs rhs) #:transparent)

(provide (struct-out Identifier) (struct-out Symbol) (struct-out String)
         (struct-out Field) (struct-out Newline) (struct-out Or)
         (struct-out Binary))
