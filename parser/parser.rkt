#lang racket

(require "structs.rkt")
(require "utilities.rkt")

(define end?
  (λ (v) (ormap (λ (pred?) (pred? v))
                (list null?
                      Newline?
                      (λ (v) (match v [(Symbol '\|) #t] [_ #f]))))))

(define parse-vals
  (λ (tokens acc)
    (match* (tokens acc)
      [((? end?) (? null?)) (error "expected a value, got nothing")]
      [(`(,(? end?) . ,_) _) (values acc tokens)]
      ;; TODO: add parsing of as keyword
      [(`(,(or (? String? tok)
               (? Identifier? tok)) . ,toks) _)
       (parse-vals toks (append acc (list tok)))])))

(define parse-bin
  (λ (tokens)
    (match/values
      (parse-vals tokens null)
      [(lhs lhs-tokens) #; (values lhs lhs-tokens)
       (make-binary lhs-tokens lhs parse-vals '(\|))
       ])))

(define parse-expr
  (λ (tokens)
    (parse-bin tokens)))

(define consume-symbol
  (λ (tokens kws)
    (match tokens
      [(? null?) (error "tried to consume an empty program")]
      [`(,token . ,other-tokens) #:when (and (Symbol? token) (member (Symbol-sym token) kws)) other-tokens]
      [_ (error "consumed wrong type")])))

(define consume-string
  (λ (tokens)
    (match tokens
      [`(,(String s) . ,other-tokens) (values (String s) other-tokens)]
      [_ (error "Unexpected Token, expected a String")])))

(define parse-stmt
  (λ (tokens)
    (match tokens
      [`(,(? Identifier? ident) . ,ident-tokens)
       (match (car ident-tokens)
         [(Symbol (or '::= '=))
          (let* [(sym (car ident-tokens))
                 (eq-tokens (consume-symbol ident-tokens (list (Symbol-sym sym))))]
            (match/values
              (match sym
                [(Symbol '::=) (parse-expr eq-tokens)]
                [(Symbol '=) (consume-string eq-tokens)])
              [(rhs rhs-tokens)
               (match rhs-tokens
                 [`(,(or (Newline) '()) . ,oth-toks) (values (match sym
                                                               [(Symbol '::=) (Field ident rhs)]
                                                               [(Symbol '=) (TokenField ident rhs)]) oth-toks)])]))]
         [_ (error "Unexpected Tokens, expected ::= or =")])]
      [`(,(? Newline?) . ,other-tokens) (parse-stmt other-tokens)]
      [_ (error "Unexpected Tokens, expected an Identifier")])))

(define parse-top
  (λ (tokens)
    (parse-stmt tokens)))

(define parse-body-tk
  (λ (tokens stmts)
    (match tokens
      ['() (values stmts tokens)]
      [_ (match/values
           (parse-top tokens)
           [(stmt post-tokens) (parse-body-tk post-tokens (append stmts (list stmt)))])])))

(define parse-body
  (λ (tokens)
    (match/values (parse-body-tk tokens null)
      [(body _) body])))

(define parse
  (λ (tokens)
    (parse-body tokens)))

(provide parse)
