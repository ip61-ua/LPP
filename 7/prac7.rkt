#lang racket
(require "lpp.rkt")
(require rackunit)

; Ejercicio 1
; a
(define lista-a '((a b) d (c (e) (f g) h)))
(check-equal? (fourth (third lista-a)) 'h)

(pinta-lista lista-a)