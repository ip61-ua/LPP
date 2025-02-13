#lang racket
(require "lpp.rkt")

; Ejercicio 1
; a

(define (menor x y)
  (if (< x y) x y))

(define (minimo l)
  (cond
    ((null? l) null)
    ((if (null? (rest l))
         (first l)
         (menor (first l) (minimo (rest l)))))
    )
 )

(minimo null) ; ⇒ 2
(minimo '(2)) ; ⇒ 2
(minimo '(1 8 6 4 3)) ; ⇒ 1
(minimo '(1 -1 3 -6 4)) ; ⇒ -6