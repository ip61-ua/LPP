#lang racket

;; Ejercicio 6

;; apartado a)
(define (cuadrado x)
  (* x x))

(define (diff x y)
  (- x y))

(define (diff-x p1 p2)
  (diff (car p1) (car p2)))

(define (diff-y p1 p2)
  (diff (cdr p1) (cdr p2)))

(define (suma-cuadrado p1 p2)
  (+ (cuadrado (diff-x p1 p2)) (cuadrado (diff-y p1 p2)) ))

(define (distancia p1 p2)
  (sqrt (suma-cuadrado p1 p2) ))

;; apartado b)
(define (list-distancias3 p1 p2 p3)
  (list (distancia p1 p2) (distancia p2 p3) (distancia p3 p1)))

(define (xor3 d1 d2 d3)
  (xor (xor (= d1 d2) (= d2 d3)) (= d3 d1)))

(define (conciden-solo2 d1 d2 d3)
  (and (xor3 d1 d2 d3) (not (= d1 d2 d3)) ))

(define (second lt)
  (first (rest lt)))

(define (third lt)
  (second (rest lt)))

(define (unpack-conciden lt)
  (conciden-solo2 (first lt) (second lt) (third lt)))

(define (isosceles? p1 p2 p3)
  (unpack-conciden (list-distancias3 p1 p2 p3) ))