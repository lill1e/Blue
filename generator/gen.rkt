#lang racket
(require (for-syntax syntax/parse))
(require (for-syntax "../parser/structs.rkt"))
(require (for-syntax "../parser.rkt"))

(begin-for-syntax
  (require racket/match)
  (require racket/syntax)
  (require racket/set)
  (require racket/list)

  (define (gen-tokenizer tokens inp)
    (define (make-rx rx)
      (regexp (string-append "^" rx)))

    (define rxs
      (for/list ([tok tokens])
                  #`(cons '#,(Identifier-ident (TokenField-lhs tok)) #,(make-rx (String-s (TokenField-rhs tok))))))

    #`(begin
        (define (find-match rxs input)
          (match rxs
            ['() (values #f #f)]
            [(cons (cons kind rx) rxs)
             (define match (regexp-match rx input))
             (if match
                 (values (cons kind match) (regexp-replace rx input ""))
                 (find-match rxs input))]))

        (define (tokenize/work rxs input)
          (cond
            [(= 0 (string-length input)) null]
            [else
             (define-values (matched replaced) (find-match rxs input))
             (cons matched (tokenize/work rxs (string-trim replaced)))]))
        
        (tokenize/work (list #,@rxs) #,inp)))

  (define (flatten-ors kind)
    (match kind
      [(Or lhs rhs)
        (append (flatten-ors lhs) (flatten-ors rhs))]
      [_ (list kind)]))

  (define (gen-parser/loop defined-tokens prefix kind tokenized consumed)
    (match kind
      ['() #`(values #,consumed null)]
      ; Tokens
      [(cons (or (Identifier item) (As (Identifier item) (Identifier _))) rst)
       #:when (set-member? defined-tokens (Identifier item))
        (define value (car kind))
        (define ret (match value
          [(Identifier _) #`rst-parsed]
          [(As (Identifier _) (Identifier field)) #`(dict-set rst-parsed '#,field (cadr token))]))
        #`(begin
            (if (null? #,tokenized)
              (values #,consumed #f)
              (let*-values ([(token) (car #,tokenized)]
                            [(token-name) (car token)]
                            [(rst-consumed rst-parsed) #,(gen-parser/loop defined-tokens prefix rst #`(cdr #,tokenized) (+ 1 consumed))])
              (if (and (equal? '#,item token-name) rst-parsed)
                  (values rst-consumed #,ret)
                  (values #,consumed #f)))
            ))]
      ; Other nodes
      [(cons (or (Identifier item) (As (Identifier item) (Identifier _))) rst)
       (define node-consumed (datum->syntax #f (gensym 'node-consumed)))
       (define node-parsed (datum->syntax #f (gensym 'node-parsed)))
       (define rst-consumed (datum->syntax #f (gensym 'rst-consumed)))
       (define rst-parsed (datum->syntax #f (gensym 'rst-parsed)))
        (define value (car kind))
        (define ret (match value
          [(Identifier _) rst-parsed]
          [(As (Identifier _) (Identifier field)) #`(dict-set #,rst-parsed '#,field #,node-parsed)]))
       (define name (format-id prefix "~a-~a" (syntax->datum prefix) item))
       (define parser (format-id prefix "~a-parse/tokens" name))
        #`(begin
          (let*-values ([(#,node-consumed #,node-parsed) (#,parser #,tokenized)]
                        [(#,rst-consumed #,rst-parsed) (if #,node-parsed #,(gen-parser/loop defined-tokens prefix rst #`(list-tail #,tokenized #,node-consumed) consumed) (values #f #f))])
          (if (and #,node-parsed #,rst-parsed)
              (values (+ #,node-consumed #,rst-consumed) #,ret)
              (values #,consumed #f))))]
      ; Or
      [(Or _ _)
        (define flattened (flatten-ors kind))
        (for/foldr ([ret #`(values 0 #f)])
                   ([fl flattened])
          (define consumed (datum->syntax #f (gensym 'consumed)))
          (define parsed (datum->syntax #f (gensym 'parsed)))
          #`(let*-values ([(#,consumed #,parsed) #,(gen-parser/loop defined-tokens prefix fl tokenized 0)])
              (if #,parsed
                (values #,consumed #,parsed)
                #,ret)))]))

  (define (build-struct-from-dict dict fields name)
    #`(#,name #,@(for/list ([field fields]) #`(dict-ref #,dict '#,field null))))

  (define (gen-parser/tokens defined-tokens prefix name kind tokens)
    (define fields (gen-struct/fields kind))
    #`(begin
        (if (null? #,tokens)
          (values 0 #f)
          #,(match kind
              [(list (or (As _ _) (Identifier _)) ...)
                #`(let-values ([(consumed parsed) #,(gen-parser/loop defined-tokens prefix kind tokens 0)])
                    (if parsed
                      (values consumed #,(build-struct-from-dict #'parsed fields name))
                      (values 0 #f)))
              ]
              [(Or _ _)
                (define flattened (flatten-ors kind))
                (for/foldr ([ret #`(values 0 #f)])
                          ([fl flattened]
                           [i (in-naturals)])
                  (define consumed (datum->syntax #f (gensym 'consumed)))
                  (define parsed (datum->syntax #f (gensym 'parsed)))
                  #`(let*-values ([(#,consumed #,parsed) #,(gen-parser/loop defined-tokens prefix fl tokens 0)])
                      (if #,parsed
                        (values #,consumed #,(build-struct-from-dict #`(dict-set #,parsed 'variant #,i) fields name))
                        #,ret)))]))))

  (define (gen-struct/fields kind)
    (remove-duplicates (match kind
      ['() null]
      [(cons (Identifier field) rst)
        (gen-struct/fields rst)]
      [(cons (As value (Identifier field)) rst)
        (append `(,field) (gen-struct/fields rst))]
      [(Or lhs rhs)
        (append '(variant) (gen-struct/fields lhs) (gen-struct/fields rhs))]
      [(As inner (Identifier field)) (cons field (gen-struct/fields inner))])))

  (define (gen-struct field prefix tokenizer defined-tokens)
    (match field
      [(Field (Identifier ident) kind)
       (define name (format-id prefix "~a-~a" (syntax->datum prefix) ident))
       (define parser (format-id prefix "~a-parse" name))
       (define parser/tokens (format-id prefix "~a-parse/tokens" name))
       (define fields (gen-struct/fields kind))
       #`(begin
           (struct #,name (#,@fields) #:transparent)
           (define (#,parser/tokens tokens)
            #,(gen-parser/tokens defined-tokens prefix name kind #'tokens))
           (define (#,parser text)
             (define-values (_ parsed) (#,parser/tokens (#,tokenizer text)))
             (unless parsed
              (error 'bad-syntax))
             parsed))])))
             
(define-syntax (generate stx)
  (syntax-parse stx
    [(_ prefix:id path:str)
     (define tokenizer (format-id #'prefix "~a-tokenize" (syntax->datum #'prefix)))
     (define inp #'tokenizer-input)

     (define parsed (file->blue (syntax->datum #'path)))
     (define tokens (filter TokenField? parsed))
     (define nodes (filter Field? parsed))

     (define defined-tokens (list->set (map TokenField-lhs tokens)))
     #`(begin
         (define (#,tokenizer #,inp)
           #,(gen-tokenizer tokens inp))
           #,@(map (lambda (x) (gen-struct x #'prefix tokenizer defined-tokens)) nodes)
         )]))

(provide generate)

