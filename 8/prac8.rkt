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

;;; 3
;;; a)

(define arbole1 '(10 (2) (12 (4) (2)) (10 (5))))
(define arbole2 '(10 (2) (12 (4) (2)) (10 (6))))

(define (hojas-cumplen-bosque pred bosque)
  (if (null? bosque)
      '()
      (append (hojas-cumplen pred (first bosque))
              (hojas-cumplen-bosque pred (rest bosque)))))

(define (hojas-cumplen pred arbol)
  (if (and (hoja-arbol? arbol) (pred (dato-arbol arbol)))
      (list (dato-arbol arbol))
      (hojas-cumplen-bosque pred (hijos-arbol arbol))))

(define (hojas-cumplen-fos pred arbol)
  (foldl (lambda (cur acc)
           (append acc (hojas-cumplen-fos pred cur)))
         (if (and (hoja-arbol? arbol)
                  (pred (dato-arbol arbol))) 
             (list (dato-arbol arbol))
             '())
         (hijos-arbol arbol)))

;(hojas-cumplen even? arbole1) ; ⇒ '(2 4 2)
(check-equal? (hojas-cumplen even? arbole1) '(2 4 2))
;(hojas-cumplen even? arbole2) ; ⇒ '(2 4 2 6)
(check-equal? (hojas-cumplen even? arbole2) '(2 4 2 6))
;(hojas-cumplen-fos even? arbole1) ; ⇒ '(2 4 2)
(check-equal? (hojas-cumplen-fos even? arbole1) '(2 4 2))
;(hojas-cumplen-fos even? arbole2) ; ⇒ '(2 4 2 6)
(check-equal? (hojas-cumplen-fos even? arbole2) '(2 4 2 6))

;;; b)

; Orgulloso de esta solución usando solamente operadores lógicos.
(define (todas-hojas-cumplen-bosque? pred bosque)
  (or (null? bosque)
      (and (todas-hojas-cumplen? pred (first bosque))
           (todas-hojas-cumplen-bosque? pred (rest bosque)))))

(define (todas-hojas-cumplen? pred arbol)
  (xor (and (not (hoja-arbol? arbol))
            (todas-hojas-cumplen-bosque? pred (hijos-arbol arbol)))
       (and (hoja-arbol? arbol)
            (pred (dato-arbol arbol)))))

(define (todas-hojas-cumplen-fos? pred arbol)
  (xor (and (not (hoja-arbol? arbol))
            (for-all? (lambda (x) (todas-hojas-cumplen-fos? pred x))
                      (hijos-arbol arbol)))
       (and (hoja-arbol? arbol)
            (pred (dato-arbol arbol)))))

(check-equal? (todas-hojas-cumplen? even? arbole1) #f)
(check-equal? (todas-hojas-cumplen? even? arbole2) #t)
(check-equal? (todas-hojas-cumplen? (lambda (x) (> x 0)) '(10 (2) (12 (4) (2)) (10 (6)))) #t)
(check-equal? (todas-hojas-cumplen? (lambda (x) (> x 0)) '(10 (2) (12 (4) (-9999)) (10 (6)))) #f)
(check-equal? (todas-hojas-cumplen? (lambda (x) (> x 0)) '(10 (2) (-999 (4) (512)) (10 (6)))) #t)

(check-equal? (todas-hojas-cumplen-fos? even? arbole1) #f)
(check-equal? (todas-hojas-cumplen-fos? even? arbole2) #t)
(check-equal? (todas-hojas-cumplen-fos? (lambda (x) (> x 0)) '(10 (2) (12 (4) (2)) (10 (6)))) #t)
(check-equal? (todas-hojas-cumplen-fos? (lambda (x) (> x 0)) '(10 (2) (12 (4) (-9999)) (10 (6)))) #f)
(check-equal? (todas-hojas-cumplen-fos? (lambda (x) (> x 0)) '(10 (2) (-999 (4) (512)) (10 (6)))) #t)

;;; 4
;;; a)
(define arbol3 '(20 (2) (8 (4) (2)) (9 (5))))

;(define (suma-raices-hijos-fail arbol)
;  (foldr (lambda (cur acc)
;           (+ acc (suma-raices-hijos cur)))
;         (if (hoja-arbol? arbol)
;             0
;             (dato-arbol arbol))
;         (hijos-arbol arbol)))

(define (suma-raices-hijos arbol)
  (foldr (lambda (cur acc)
           (+ acc (dato-arbol cur)))
         0
         (hijos-arbol arbol)))

(check-equal? (suma-raices-hijos arbol3) 19) ; ⇒ 19
(check-equal? (suma-raices-hijos (second (hijos-arbol arbol3))) 6) ; ⇒ 6

;;; b)
(define (raices-mayores-arbol-sumador bosque)
  (if (null? bosque) 0
      (+ (dato-arbol (first bosque))
         (raices-mayores-arbol-sumador (rest bosque)))))

(define (raices-mayores-bosque? bosque)
  (or (null? bosque)
      (and (raices-mayores-arbol? (first bosque))
           (raices-mayores-bosque? (rest bosque)))))

(define (raices-mayores-arbol? arbol)
  (and (> (dato-arbol arbol)
          (raices-mayores-arbol-sumador (hijos-arbol arbol)))
       (raices-mayores-bosque? (hijos-arbol arbol))))

(define (raices-mayores-arbol-fos? arbol)
  (and (> (dato-arbol arbol)
          (foldr (lambda (cur acc)
                   (+ acc (dato-arbol cur)))
                   0
                   (hijos-arbol arbol)))
       (for-all? raices-mayores-arbol-fos? (hijos-arbol arbol))))

(define arbol4 '(20 (2) (8 (4) (5)) (9 (5))))
(check-equal? (raices-mayores-arbol? arbol3) #t)
(check-equal? (raices-mayores-arbol? arbol4) #f)
(check-equal? (raices-mayores-arbol-fos? arbol3) #t)
(check-equal? (raices-mayores-arbol-fos? arbol4) #f)

;;; c)
(define (comprueba-raices-bosque bosque)
  (if (null? bosque) '()
      (cons (comprueba-raices-arbol (first bosque))
            (comprueba-raices-bosque (rest bosque)))))

(define (comprueba-raices-arbol arbol)
  (construye-arbol (if (or (hoja-arbol? arbol)
                           (> (dato-arbol arbol)
                              (raices-mayores-arbol-sumador (hijos-arbol arbol))))
                       1
                       0)
                   (comprueba-raices-bosque (hijos-arbol arbol)) ))

(define (comprueba-raices-arbol-fos arbol)
  (construye-arbol (if (or (hoja-arbol? arbol)
                           (> (dato-arbol arbol)
                              (foldr (lambda (cur acc)
                                       (+ acc (dato-arbol cur)))
                                     0
                                     (hijos-arbol arbol))))
                       1
                       0)
                   (map comprueba-raices-arbol-fos
                        (hijos-arbol arbol))))

;(pinta-arbol (comprueba-raices-arbol arbol4))
(check-equal? (comprueba-raices-arbol arbol3) '(1 (1) (1 (1) (1)) (1 (1)))) ; ⇒ (1 (1) (1 (1) (1)) (1 (1)))
(check-equal? (comprueba-raices-arbol arbol4) '(1 (1) (0 (1) (1)) (1 (1)))) ; ⇒ (1 (1) (0 (1) (1)) (1 (1)))
(check-equal? (comprueba-raices-arbol-fos arbol3) '(1 (1) (1 (1) (1)) (1 (1)))) ; ⇒ (1 (1) (1 (1) (1)) (1 (1)))
(check-equal? (comprueba-raices-arbol-fos arbol4) '(1 (1) (0 (1) (1)) (1 (1)))) ; ⇒ (1 (1) (0 (1) (1)) (1 (1)))

;;; 5
;;; a)

(define arbol-quantico (construye-arbol 'a
                                        (list (construye-arbol 'a
                                                               (list (construye-arbol 'a '())
                                                                     (construye-arbol 'b '())))
                                              (construye-arbol 'b
                                                               (list (construye-arbol 'a '())
                                                                     (construye-arbol 'c '())))
                                              (construye-arbol 'c '()))))
;(pinta-arbol arbol-quantico)

(define (es-camino-bosque? l bosque)
  (and (not (null? bosque))
       (or (es-camino? l (first bosque))
           (es-camino-bosque? l (rest bosque)))))

(define (es-camino? lista arbol)
  (if (and (not (hoja-arbol? arbol)) (null? (rest lista)))
      #f
      (or (and (hoja-arbol? lista)
               (equal? (dato-arbol arbol)
                       (first lista)))
          (and (equal? (dato-arbol arbol)
                       (first lista))
               (es-camino-bosque? (rest lista)
                                  (hijos-arbol arbol))))))

(check-equal? (es-camino? '(a b a) arbol-quantico) #t) ; ⇒ #t
(check-equal? (es-camino? '(a b) arbol-quantico) #f) ; ⇒ #f
(check-equal? (es-camino? '(a b a b) arbol-quantico) #f) ; ⇒ #f



