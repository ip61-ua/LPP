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
  null)

(define (rota-indice indice desplazamiento)
  null)

(define (cifra-caracter char desplazamiento)
  null)

(define (descifra-caracter char desplazamiento)
  null)

(encuentra-caracter 0)
