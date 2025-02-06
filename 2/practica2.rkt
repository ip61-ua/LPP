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
  (if (entre-az? char)
      (if (entre-az? (encuentra-caracter (+ indice desplazamiento)))
          (+ indice desplazamiento)
          (modulo (+ indice desplazamiento) (+ (encuentra-indice #\z) 1)))
      indice))
  

(define (perform-rotation c d)
  (if (char-upper-case? c)
      (char-upcase (encuentra-caracter (rota-indice (encuentra-indice (char-downcase c)) d)))
      (encuentra-caracter (rota-indice (encuentra-indice c) d))))

(define (cifra-caracter char desplazamiento)
  (perform-rotation char desplazamiento)
        
(define (descifra-caracter char desplazamiento)
  (perform-rotation char (- desplazamiento))

(cifra-caracter #\c 5) ; ⇒ #\h)
(cifra-caracter #\z -1) ; ⇒ #\y)
(cifra-caracter #\j 40) ; ⇒ #\x)
(cifra-caracter #\D 3) ; ⇒ #\G)
(cifra-caracter #\ñ 3) ; ⇒ #\ñ)

(descifra-caracter #\d 3) ; ⇒ #\a)
(descifra-caracter #\y -1) ; ⇒ #\z)
(descifra-caracter #\x 40) ; ⇒ #\j)
(descifra-caracter #\G 3) ; ⇒ #\D)
(descifra-caracter #\tab 3) ; ⇒ #\tab)