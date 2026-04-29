#lang racket

(require "structs.rkt")
(require "utilities.rkt")

(define ops
  (list '::= '\| '=))

(define char-ops (map (λ (sym-op) (string-ref (symbol->string sym-op) 0)) ops))

(define list-op?
  (λ (loc) (op? (string->symbol (list->string loc)))))

(define op?
  (λ (op) (memv op ops)))

(define word-char?
  (λ (c)
    (not (member c char-ops))))

(define word-string?
  (λ (loc)
    (and (>= (length loc) 1)
         (eqv? (car loc) #\"))))

(define chars?
  (λ (loc)
    (= (length (filter char? loc)) (length loc))))

(define chars-op?
  (λ (op)
    (if (null? op) #f
        (if (chars? op) (memv (string->symbol (list->string op)) ops) #f))))

(define op-char?
  (λ (c) (memv c char-ops)))

(define op-chars?
  (λ (chars)
    (andmap op-char? chars)))

(define value->lexeme
  (λ (s)
    (match s
      [(? string?) (String (substring s 1 (sub1 (string-length s))))]
      ['|\n| (Newline)]
      [(? op?) (Symbol s)]
      [v #:when (symbol? v)
       (match v
         ['as (Keyword 'as)]
         [_ (Identifier v)])]
      [_ (error "invalid char found")])))

(define word?
  (λ (loc)
    (cond
      [(list? loc)
       (let* [(str (list->string loc))
              (sym (string->symbol str))]
         (cond
           [(ormap op? loc) #f]
           [(memv sym ops) #f]
           [(= (string-length str) 0) #t]
           [else #t]))]
      [else #f])))

(define acc->sym
  (λ (acc)
    (match acc
      ['|\n| acc]
      [(or (? list-op?) (? word?)) (string->symbol (apply string acc))])))

(define extend-acc
  (λ (acc item)
    (append (list-if-needed acc)
            (if (null? item)
                null
                (list
                  (if
                    (and (list? item)
                         (eq? #\" (car item)))
                    (list->string item) (acc->sym item)))))))

(define file->symbols
  (λ (p acc alt-acc)
    (let [(r (read-char p))]
      (match* (r alt-acc)
        [(_ _) #:when (eof-object? r) (if (eq? (last acc) '|\n|) (extend-acc acc alt-acc) (extend-acc (extend-acc acc alt-acc) '|\n|))]
        [(#\space _) #:when (not (word-string? alt-acc)) (file->symbols p (extend-acc acc alt-acc) null)]
        [(#\newline _) (file->symbols p (extend-acc (extend-acc acc alt-acc) '|\n|) null)]
        [(#\" (? word-string?)) #:when (or (empty? alt-acc) (not (equal? (last alt-acc) #\\)))
          (file->symbols p (extend-acc acc (append alt-acc (list r))) null)]
        [(#\" _) #:when (or (empty? alt-acc) (not (equal? (last alt-acc) #\\)))
          (file->symbols p (extend-acc acc alt-acc) (list r))]
        [((? char?) (? word-string?))
        
          (file->symbols p acc (append alt-acc (list r)))]
        [((? word-char?) (? word?)) (file->symbols p acc (append alt-acc (list r)))]
        [((? word-char?) _) (file->symbols p (extend-acc acc alt-acc) (list r))]
        [((? char?) _) #:when (chars-op? (append (list-if-needed alt-acc) (list r))) (file->symbols p acc (append alt-acc (list r)))]
        [((? char?) (? op?)) (file->symbols p acc (append alt-acc (list r)))]
        [((? char?) (? op-chars?)) (file->symbols p acc (append alt-acc (list r)))]
        [((? char?) _) (file->symbols p (extend-acc acc alt-acc) (list r))]))))

(define lex
  (λ (file-name) (map value->lexeme (file->symbols (open-input-file file-name) null null))))

(provide lex)
