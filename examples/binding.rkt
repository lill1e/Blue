#lang racket
(require "../blue.rkt")
(generate lang "./binding.blue")

(lang-binding-parse "let x = 2;")



