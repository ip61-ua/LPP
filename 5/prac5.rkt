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

;(mueve-al-principio-condicion number? '(a b)) ; ⇒ '(a b)
;(mueve-al-principio-condicion number? '(a b c 1 d 1 e)) ; ⇒ (1 a b c d 1 e)
;(mueve-al-principio-condicion number? '(1 a b 1 c)) ; ⇒ (1 a b 1 c)
;(mueve-al-principio-condicion number? '(a b c d)) ; ⇒ '(a b c d)

;; c)

(define (aux-check p a b)
  (if (p (first a) (first b))
      (cons (first a) (first b))
      '()))

(define (elimina-vacio par)
  (if (null? (first par))
      (rest par)
      par))

(define (comprueba pred lista1 lista2)
  (if (or (null? lista1) (null? lista2))
      '()
      (elimina-vacio (cons (aux-check pred lista1 lista2)
                           (comprueba pred (rest lista1) (rest lista2))))))

;(comprueba (lambda (x y)
;             (= (string-length (symbol->string x)) y))
;           '(este es un ejercicio de examen) 
;           '(2 1 2 9 1 6))
; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))

;(comprueba (lambda (x y)
;              (= (string-length x) (string-length y)))
;             '("aui" "a" "ae" "c" "aeiou")
;             '("hola" "b" "es" "que" "cinco"))
; ⇒ (("a" . "b") ("ae" . "es") ("aeiou" . "cinco"))

;; Ejercicio 2


;----;
; b) ;
;----;

(define (inserta-ordenada n lista-ordenada)
  (cond
    ((null? lista-ordenada) (list n))
    ((<= n (first lista-ordenada)) (cons n lista-ordenada))
    (else (cons (first lista-ordenada)
                (inserta-ordenada n (rest lista-ordenada))))))

(inserta-ordenada 10 '(-8 2 3 11 20)) ;-> '(-8 2 3 10 11 20)

(define (inserta-ordenada-genérica n lista-ordenada menor-igual?)
  ...)

;----;
; c) ;
;----;


(define (ordena lista)
  (if (null? lista)
      '()
      (inserta-ordenada (first lista) (ordena (rest lista)))))

(ordena '(2 -1 100 4 -6)) ;-> '(-6 -1 2 4 100)

(define (ordena lista menor-igual?) ...)
