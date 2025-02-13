#lang racket
(require "lpp.rkt")

; Ejercicio 1
; a.1

(define (menor x y)
  (if (< x y) x y))

(define (minimo l)
  (cond
    ((null? l) '())
    ((if (null? (rest l))
         (first l)
         (menor (first l) (minimo (rest l)))))
    )
 )

; Ejemplos
; (minimo null) ; ⇒ ()
; (minimo '(2)) ; ⇒ 2
; (minimo '(1 8 6 4 3)) ; ⇒ 1
; (minimo '(1 -1 3 -6 4)) ; ⇒ -6

; a.2

(minimo '(1 8 6 4 3)) ; ⇒ 1

; ¿Qué lista se pasa como parámetro a la primera llamada recursiva a la función?
; > Se pasa (rest l), siendo en eset caso: ( 8 6 4 3 )

; ¿Qué devuelve esa llamada recursiva?
; > Un numero cuya condición es ser el menor de (rest l). Siguiendo con el ejemplo anterior sería 3. 

; ¿Con qué argumentos se llama a la función menor que devuelve el resultado final?
; > (first l) (minimo (rest l));
;   1 (minimo ( 8 6 4 3 ));
;   1 3;



