#lang racket
(require "lpp.rkt")

; Ejercicio 1
; a.1)

;;;; IMPORTADO DE PRÁCTICA 2
(define (menor x y)
  (if (< x y) x y))
;;;; FIN IMPORT

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

; a.2)

; (minimo '(1 8 6 4 3)) ; ⇒ 1

; ¿Qué lista se pasa como parámetro a la primera llamada recursiva a la función?
; > Se pasa (rest l), siendo en eset caso: ( 8 6 4 3 )

; ¿Qué devuelve esa llamada recursiva?
; > Un numero cuya condición es ser el menor de (rest l). Siguiendo con el ejemplo anterior sería 3. 

; ¿Con qué argumentos se llama a la función menor que devuelve el resultado final?
; > (first l) (minimo (rest l));
;   1 (minimo ( 8 6 4 3 ));
;   1 3;

; b)

(define (concatena l)
  (if (null? l)
      ""
      (string-append (string (first l)) (concatena (rest l)) )))

; (concatena '()) ; ⇒ ""
; (concatena '(#\H #\o #\l #\a)) ; ⇒ "Hola"
; (concatena '(#\S #\c #\h #\e #\m #\e #\space #\m #\o #\l #\a)) ; ⇒ "Scheme mola"

; c)

;;;; IMPORTADO DE PRÁCTICA 2
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

;;;; FIN IMPORT

(define (cifra-cadena cad desplazamiento)
  (if (= (string-length cad) 0)
      ""
      (cifrar-recursivamente cad desplazamiento)))

(define (cifrar-recursivamente cad despl)
  (string-append (string (cifra-caracter (string-ref cad 0) despl)) (cifra-cadena (substring cad 1) despl)))

(define (descifra-cadena cad desplazamiento)
  (cifra-cadena cad (- desplazamiento)))

; (cifra-cadena "En un lugar de la Mancha, de cuyo nombre no quiero acordarme" 10)
; ⇒ "Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo"

; (descifra-cadena "Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo" 10)
; ⇒ "En un lugar de la Mancha, de cuyo nombre no quiero acordarme"

; d)

(define (es-primero? l x)
  (equal? (first l) x))

(define (hay-algo? l)
  (not (null? l)))

(define (es-resto? l x)
  (contiene? (rest l) x))

(define (contiene? l x)
  (and (hay-algo? l)
       (or (es-primero? l x)
           (es-resto? l x))))

(define (str-contiene? str x)
  (contiene? (string->list str) x))

; (contiene? '(algo 3 #\A) 3) ; ⇒ #t
; (contiene? '(algo 3 #\A) "algo") ; ⇒ #f
; (contiene? '(algo 3 #\A) 'algo) ; ⇒ #t
; (str-contiene? "Hola" #\o) ; ⇒ #t
; (str-contiene? "Esto es una frase" #\space) ; ⇒ #t
; (str-contiene? "Hola" #\h) ; ⇒ #f

; Ejercicio 2

; a)

(define (primeros2-iguales? l)
  (equal? (first l) (second l)))

(define (todos-iguales? lista)
  (or (null? lista)
      (null? (rest lista))
      (and (primeros2-iguales? lista)
           (todos-iguales? (rest lista)) )))

; (todos-iguales? '()) ; ⇒ #t
; (todos-iguales? '(a)) ; ⇒ #t
; (todos-iguales? '(a a a a a a a)) ; ⇒ #t
; (todos-iguales? '((a b) (a b) (a b))) ; ⇒ #t
; (todos-iguales? '(a a a a a b)) ; ⇒ #f

; b
(define (no-hay-otro-1ro-igual? l)
  (not (contiene? (rest l) (first l))))

(define (todos-distintos? lista)
  (or (null? lista)
      (null? (rest lista))
      (and (no-hay-otro-1ro-igual? lista)
           (todos-distintos? (rest lista)))))

; (todos-distintos? '()) ; ⇒ #t
; (todos-distintos? '(a)) ; ⇒ #t
; (todos-distintos? '(a b c)) ; ⇒ #t
; (todos-distintos? '(a b c a)) ; ⇒ #f

; c


(define (solo-dos-iguales? lista)
  (or (not (null? lista))
      (and (not (null? lista)) (not (null? (rest lista))) )
      (and 
           (todos-distintos? (rest lista)))))

(solo-dos-iguales? '()) ; ⇒ #f
(solo-dos-iguales? '(a)) ; ⇒ #f
(solo-dos-iguales? '(a b c a)) ; ⇒ #t
(solo-dos-iguales? '(a b c b a a)) ; ⇒ #f
(solo-dos-iguales? '(a b c a a)) ; ⇒ #f
(solo-dos-iguales? '(a b c a b)) ; ⇒ #f



