#lang racket
(require "lpp.rkt")
(require rackunit)

;;; 1
;;; a.1)
(define arbol-sin-barrera '(15 (4 (2)
                                  (3))
                               (8 (6))
                               (12 (9)
                                   (10)
                                   (11))))
;(pinta-arbol arbol-sin-barrera)

(define arbol
  (construye-arbol 15
                   (list (construye-arbol 4  (list (construye-arbol 2 '())
                                                   (construye-arbol 3 '())))
                         (construye-arbol 8  (list (construye-arbol 6 '())))
                         (construye-arbol 12 (list (construye-arbol 9 '())
                                                   (construye-arbol 10 '())
                                                   (construye-arbol 11 '()))))))
;(pinta-arbol arbol)
;(hoja-arbol? (construye-arbol 11 '()))

(check-equal? arbol arbol-sin-barrera)
(check-equal? (first (second (rest (third (rest arbol-sin-barrera))))) 10)
(check-equal? (first (second (hijos-arbol (third (hijos-arbol arbol))))) 10)

;;; a.2)
(define (suma-datos-arbol arbol)
    (+ (dato-arbol arbol)
       (suma-datos-bosque (hijos-arbol arbol))))

(define (suma-datos-bosque bosque)
    (if (null? bosque)
        0
        (+ (suma-datos-arbol (first bosque)) 
           (suma-datos-bosque (rest bosque)))))

;(suma-datos-bosque (hijos-arbol arbol))

;;; ¿Qué devuelve la invocación a (suma-datos-arbol (first bosque)) que se
;;;  realiza dentro de la función?
;;> Un número. La suma de los elementos de un arbol. Es decir, se recibe un bosque
;;> (lista de árboles) y devuelve la suma de los elementos del primer arbol del bosque.

;;; ¿Qué devuelve la primera llamada recursiva a suma-datos-bosque?
;;> Un número. Efectuadon la recursión al resto del bosque.

;;; a.3)
(define (suma-datos-arbol-fos arbol)
   (foldr +
          (dato-arbol arbol) 
          (map suma-datos-arbol-fos (hijos-arbol arbol))))

;(suma-datos-arbol-fos arbol)

;;; ¿Qué devuelve la invocación a map dentro de la función?
;;> Un lista de números que refleja la suma de cada uno de los hijos.
;;; ¿Qué invocaciones se realizan a la función + durante la ejecución de foldr sobre
;;;  la lista devuelta por la invocación a map? Enuméralas en orden, indicando sus
;;;  parámetros y el valor devuelto en cada una de ellas.
;;> 1. (+ 15 9), 9 = 4+3+2
;;> 2. (+ 14 (+ 15 9)), 14 = 8+6
;;> 3. (+ 42 (+ 14 (+ 15 9))), 42 = 12+9+10+11


;;; b.1)
(define arbolb-sin-barrera '(40 (23 (5 () ())
                                    (32 (29 () ())
                                        ()))
                                (45 ()
                                    (56 () ()))))
;(pinta-arbolb arbolb-sin-barrera)

(define arbolb (construye-arbolb 40
                                 (construye-arbolb 23
                                                   (construye-arbolb 5
                                                                     arbolb-vacio
                                                                     arbolb-vacio)
                                                   (construye-arbolb 32
                                                                     (construye-arbolb 29
                                                                                       arbolb-vacio
                                                                                       arbolb-vacio)
                                                                     arbolb-vacio))
                                 (construye-arbolb 45
                                                   arbolb-vacio
                                                   (construye-arbolb 56
                                                                     arbolb-vacio
                                                                     arbolb-vacio))))
;(pinta-arbolb arbolb)
(check-equal? arbolb arbolb-sin-barrera)
(check-equal? (dato-arbolb (hijo-izq-arbolb (hijo-der-arbolb (hijo-izq-arbolb arbolb)))) 29)
