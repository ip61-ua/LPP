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

;;; 2
;;; a)
(define arbol2 '(a (b (c (d)) (e)) (f)))

(define (to-string-arbol-bosque bosque)
  (if (null? bosque)
      ""
      (string-append (to-string-arbol (first bosque))
                     (to-string-arbol-bosque (rest bosque)))))

(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (to-string-arbol-bosque (hijos-arbol arbol))))

(define (to-string-arbol-fos arbols)
  (foldl (lambda (cur acc)
           (string-append acc
                          (to-string-arbol-fos cur) ))
         (symbol->string (dato-arbol arbols))
         (hijos-arbol arbols)))

;(to-string-arbol-fos arbol2) ; ⇒ "abcdef"
(check-equal? (to-string-arbol arbol2) "abcdef")

;;; b)
(define (veces-arbol-bosque dato bosque)
  (if (null? bosque)
      0
      (+ (veces-arbol dato (first bosque))
         (veces-arbol-bosque dato (rest bosque)))))

(define (veces-arbol dato arbol)
  (+ (veces-arbol-bosque dato (hijos-arbol arbol))
     (if (equal? dato (dato-arbol arbol))
         1
         0)))

(define (veces-arbol-fos dato arbol)
  (foldr (lambda (cur acc) (+ acc (veces-arbol-fos dato cur)))
         (if (equal? dato (dato-arbol arbol)) 1 0)
         (hijos-arbol arbol)))

;(veces-arbol 'b '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 3
(check-equal? (veces-arbol 'b '(a (b (c) (d)) (b (b) (f)))) 3)
;(veces-arbol 'g '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 0
(check-equal? (veces-arbol 'g '(a (b (c) (d)) (b (b) (f)))) 0)
;(veces-arbol-fos 'b '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 3
(check-equal? (veces-arbol-fos 'b '(a (b (c) (d)) (b (b) (f)))) 3)
;(veces-arbol-fos 'g '(a (b (c) (d)) (b (b) (f)))) ; ⇒ 0
(check-equal? (veces-arbol-fos 'g '(a (b (c) (d)) (b (b) (f)))) 0)

 
















