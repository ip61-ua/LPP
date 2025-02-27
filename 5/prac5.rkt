#lang racket

;; Ejercicio 1

;; a)

(define (aplica-veces f1 f2 n x)
  (if (= n 0)
      x
      (aplica-veces f1 f2 (- n 1) (f1 (f2 x)) )))

;(aplica-veces (lambda (x) (* 2 x)) (lambda (x) (+ 2 x)) 3 5) ; ⇒ 68
;(aplica-veces (lambda (x) (+ x 1)) (lambda (x) (+ x 2)) 2 10) ; ⇒ 16
;(aplica-veces (lambda (x) (* x x)) (lambda (x) (+ x 1)) 4 3) ; ⇒ 7072978201

;; b)

(define (intercambia-dos-primeros pred l)
  (if (pred (second l))
      (cons (second l) (cons (first l) (rest (rest l))))
      l))

(define (mueve-al-principio-condicion pred lista)
  (cond
    ((or (null? lista)
          (null? (rest lista))
          (pred (first lista))) lista)
    ((pred (second lista)) (intercambia-dos-primeros pred lista))
    (else (intercambia-dos-primeros pred (cons (first lista)
                                                (mueve-al-principio-condicion pred (rest lista)))))))

(mueve-al-principio-condicion number? '(a b)) ; ⇒ '(a b)
(mueve-al-principio-condicion number? '(a b c 1 d 1 e)) ; ⇒ (1 a b c d 1 e)
(mueve-al-principio-condicion number? '(1 a b 1 c)) ; ⇒ (1 a b 1 c)
(mueve-al-principio-condicion number? '(a b c d)) ; ⇒ '(a b c d)