#lang racket

;; Ejercicio 1 a
(define (binario-a-decimal b3 b2 b1 b0)
  (+ (* b3 (expt 2 3)) (* b2 (expt 2 2)) (* b1 2) (* b0 1) ))

;; Ejercicio 1 b
(define (dec->hex b)
  (if (>= b 10)
      (integer->char (+ (- b 10) (char->integer #\A)))
      (integer->char (+ b (char->integer #\0)))))

(define (binario-a-hexadecimal b3 b2 b1 b0)
  (dec->hex (binario-a-decimal b3 b2 b1 b0)) )

;; Ejercicio 2
(define (encuentra-indice char)
  (- (char->integer char) (char->integer #\a)))

(define (encuentra-caracter indice)
  (integer->char (+ indice (char->integer #\a)) ))

(define (entre-az? char)
  (<= (encuentra-indice #\a) (encuentra-indice char) (encuentra-indice #\z)))

(define (rota-indice indice desplazamiento)
  (if (entre-az? (encuentra-caracter indice))
      (if (entre-az? (encuentra-caracter (+ indice desplazamiento)))
          (+ indice desplazamiento)
          (modulo (+ indice desplazamiento) (+ (encuentra-indice #\z) 1)))
      indice))
  
(define (perform-rotation c d)
  (if (char-upper-case? c)
      (char-upcase (encuentra-caracter (rota-indice (encuentra-indice (char-downcase c)) d)))
      (encuentra-caracter (rota-indice (encuentra-indice c) d))))

(define (cifra-caracter char desplazamiento)
  (perform-rotation char desplazamiento))
        
(define (descifra-caracter char desplazamiento)
  (perform-rotation char (- desplazamiento)))

;(cifra-caracter #\c 5) ; ⇒ #\h)
;(cifra-caracter #\z -1) ; ⇒ #\y)
;(cifra-caracter #\j 40) ; ⇒ #\x)
;(cifra-caracter #\D 3) ; ⇒ #\G)
;(cifra-caracter #\ñ 3) ; ⇒ #\ñ)

;(descifra-caracter #\d 3) ; ⇒ #\a)
;(descifra-caracter #\y -1) ; ⇒ #\z)
;(descifra-caracter #\x 40) ; ⇒ #\j)
;(descifra-caracter #\G 3) ; ⇒ #\D)
;(descifra-caracter #\tab 3) ; ⇒ #\tab)

; Ejercicio 3
(define (menor-de-tres n1 n2 n3)
  (if (<= n1 n2)
      (if (<= n1 n3) n1 n3)
      (if (<= n2 n3) n2 n3)))

(define (menor x y)
  (if (< x y) x y))

(define (menor-de-tres-v2 n1 n2 n3)
  (menor n1 (menor n2 n3)))

; Ejercicio 4 a
(define (f x)
  (cons x 2))

(define (g x y)
    (cons x y))

;(g (f (+ 2 1)) (+ 1 1))
; Orden aplicativo
; (g (f 3) 2) ; R3
; (g (cons 3 2) 2) ; R4
; (cons (cons 3 2) 2); R4

; Orden normal
; (cons (f (+ 2 1)) (+ 1 1)) ; R4
; (cons (cons (+ 2 1) 2) (+ 1 1)) ; R4
; (cons (cons 3 2) 2) ; R3

; Ejercicio 4 b
(define (func-1 x)
    (/ x 0))

(define (func-2 x y)
    (if (= x 0)
        0
        y))

; (func-2 0 (func-1 10))
; Orden aplicativo
; (func-2 0 (/ 10 0)) ; R4
; (func-2 0 [err]) ; R3
; [err]

; Orden normal
; (if (= 0 0) 0 (func-1 10) ) ; R4
; 0 ; R3

; Ejercicio 5
(define (get-bigger-len x y)
  (if (>= (string-length x) (string-length y)) x y))

(define (get-1b a b)
  (get-bigger-len (first a) (first b)))

(define (get-2b a b)
  (get-bigger-len (second a) (second b)))

(define (get-3b a b)
  (get-bigger-len (third a) (third b)))

(define (cadenas-mayores a b)
  (list (get-1b a b) (get-2b a b) (get-3b a b)))

;(cadenas-mayores '("hola" "que" "tal") '("meme" "y" "adios")) ; ⇒ ("hola" "que" "adios")
;(cadenas-mayores '("esto" "es" "lpp") '("hoy" "hay" "clase")) ; ⇒ ("esto" "hay" "clase")

; Ejercicio 6 a
(define tres-de-picas '3♠)
(define as-de-corazones 'A♥)
(define jota-de-diamantes 'J♦)

(define (obten-valor char)
  (cond
    ((equal? char #\A) 1)
    ((equal? char #\J) 10)
    ((equal? char #\Q) 11)
    ((equal? char #\K) 12)
    (else (- (char->integer char) (char->integer #\0)))))

(define (obten-palo char)
  (cond
    ((equal? char #\♠) 'Picas)
    ((equal? char #\♣) 'Tréboles)
    ((equal? char #\♥) 'Corazones)
    ((equal? char #\♦) 'Diamantes)))

(define (cs->p s)
  (cons (string-ref (symbol->string s) 0) (string-ref (symbol->string s) 1) ))

(define (carta c)
  (cons (obten-valor (car (cs->p c))) (obten-palo (cdr (cs->p c)))))

; (carta tres-de-picas) ; ⇒ (3 . Picas)
; (carta as-de-corazones) ; ⇒ (1 . Corazones)
; (carta 'K♣) ; ⇒ (12 . Tréboles)

; (obten-palo #\♠) ; ⇒ Picas
; (obten-palo #\♥) ; ⇒ Corazones
; (obten-valor #\3) ; ⇒ 3
; (obten-valor #\J) ; ⇒ 10

;;Ejercicio 6 b
(define (hay-trio c1 c2 c3)
  (= (car (carta c1)) (car (carta c2)) (car (carta c3))))

(define (hay-duo c1 c2)
  (= (car (carta c1)) (car (carta c2))))

(define (about-card x)
  (number->string (car (carta x))))

(define (say-x str c)
   (string-append str (about-card c)))

(define (say-trio x) (say-x "trío de " x))
(define (say-duo x) (say-x "pareja de " x))

(define (jugada-mano carta1 carta2 carta3)
  (cond
    ((hay-trio carta1 carta2 carta3) (say-trio carta1))
    ((hay-duo carta1 carta2) (say-duo carta1))
    ((hay-duo carta1 carta3) (say-duo carta1))
    ((hay-duo carta2 carta3) (say-duo carta2))
    (else "nada")))

; (symbol->string carta1) (symbol->string carta2)

; (jugada-mano '3♥ '3♣ '3♥) ; ⇒ "trío de 3"
; (jugada-mano 'K♦ '7♠ 'K♥) ; ⇒ "pareja de 12"
; (jugada-mano '5♣ '4♣ '6♣) ; ⇒ "nada"