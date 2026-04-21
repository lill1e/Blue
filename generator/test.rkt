#lang racket
(require "./gen.rkt")

(generate lang "../example.blue")
(lang-BinNum-parse "101")