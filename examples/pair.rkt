#lang racket

(require "../blue.rkt")
(generate pairs "./pair.blue")

(define pair->list
  (λ (p)
    (match p
      [(pairs-pair 1 '() '() n-str) (string->number n-str)]
      [(pairs-pair 0 fst snd '()) (list (pair->list fst) (pair->list snd))])))

(pair->list (pairs-pair-parse "(42 , 67)"))
(pair->list (pairs-pair-parse "((21, 21) , 67)"))
