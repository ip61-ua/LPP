#lang racket

(require "lpp.rkt")

;-------------
;  2011 - 12  
;-------------

;; Ejercicio 2
; la original
(define p (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                (cons (cons 2 '())
                      (cons 1
                            (cons (cons 2 '())
                                  2)))))
; la traducida de la original a lists
(define p1 (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                 (cons (list 2)
                       (cons 1
                             (cons (list 2) 2)))))
; la modificada
(define p2 (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                (cons (cons 2 '())
                      (cons 1
                            (cons 2
                                  (cons 2 '())
                                  )))))
; la modificada traducida
(define p3 (list (cons 5
                      (cons 7
                            (cons 8 7)))
                 (list 2)
                 1
                 2
                 2))

;(caja-puntero p)
;(caja-puntero p1)
;(caja-puntero p2)
;(caja-puntero p3)

; b) No es una lista porque el puntero que apunta al primer bloque
;    no puede definirse como tal.
;
;    A modo de estrategia, podemos fijarnos en las cajas que no
;    tienen punteros (aristas) salientes:
;    [8, 7]: no puede ser el final de una lista porque no termina
;     en lista vacía ('())
;    primer [2, '()]: es el final de una lista, por lo que puede
;     describirse como (list 2) al menos. Sin embargo, su puntero
;     entrante sale del primer elemento del bloque superior. Tratándose
;     así de (list 2).
;    segundo [2, '()]: (list 2) por el mismo razonamiento.
;
;    Véase las expresiones originales que se obtienen con p y p1.
;    Véase expresiones para considerar como lista a p2 y p3.
;
;    Pese a esto, el esquema sí contiene listas pero p *no apunta
;    directamente* a una de estas listas. Por lo que no podemos
;    considerar que p sea una lista, sino una pareja.

;; Ejercicio 3

; Funciones del ejemplo
(define (cuadrado x) (* x x))
(define (doble x) (* 2 x))
(define (suma-3 x) (+ 3 x))

; Resolución FOS
(define (mi-aplica-funcs n)
  (lambda (f) (f n)))

(define (resultados-funcs lista-funcs n)
  (map (mi-aplica-funcs n) lista-funcs)) ;; IMPORTANTE: (map <fn> <list>)

; Resolución recursiva
(define (mi-aplica-funcs-v2 l n)
  ((first l) n))

(define (volver-resultados-funcs-v2 l n)
  (resultados-funcs-v2 (rest l) n))

(define (resultados-funcs-v2-recursivo l n)
  (cons (mi-aplica-funcs-v2 l n)
        (volver-resultados-funcs-v2 l n)))

(define (resultados-funcs-v2 lista-funcs n)
  (if (null? lista-funcs)
      '()
      (resultados-funcs-v2-recursivo lista-funcs n)))

; Test
(resultados-funcs-v2 (list cuadrado doble suma-3) 6) ;=> (36 12 9)