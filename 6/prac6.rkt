#lang racket
(require "lpp.rkt")

; 1a
(define (concat-iter l acc)
  (if (null? l)
      acc
      (concat-iter (rest l) (string-append acc (first l)))))
  
(concat-iter '("hola" "y" "adiós") "") ; ⇒ "holayadiós"



