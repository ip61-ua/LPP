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

(minimo '(1 8 6 4 3)) ; ⇒ 1

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

(cifra-cadena "En un lugar de la Mancha, de cuyo nombre no quiero acordarme" 10)
; ⇒ "Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo"

(descifra-cadena "Ox ex veqkb no vk Wkxmrk, no meiy xywlbo xy aesoby kmybnkbwo" 10)
; ⇒ "En un lugar de la Mancha, de cuyo nombre no quiero acordarme"