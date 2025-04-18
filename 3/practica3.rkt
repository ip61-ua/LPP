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

; b

(define (concatena l)
  (if (null? l)
      ""
      (string-append (string (first l)) (concatena (rest l)) )))

; (concatena '()) ; ⇒ ""
; (concatena '(#\H #\o #\l #\a)) ; ⇒ "Hola"
; (concatena '(#\S #\c #\h #\e #\m #\e #\space #\m #\o #\l #\a)) ; ⇒ "Scheme mola"

; c

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

; d

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

; a

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

; c (PREGUNTAR EN CLASE)

(define (longitud-mas-2? l)
  (and (not (null? l))
       (not (null? (rest l))) ))

(define (cuantos-hay l x)
  (if (null? l)
      0
      (if (equal? (first l) x)
          (+ 1 (cuantos-hay (rest l) x))
          (cuantos-hay (rest l) x) )))

(define (elimina-dup l)
  (if (null? l)
      '()
      (if (= (cuantos-hay l (first l)) 1)
          (append (list (first l))
                  (elimina-dup (rest l)))
          (elimina-dup (rest l)))))

(define (get-tam l)
  (if (null? l) 0 (+ 1 (get-tam (rest l)))))

(define (solo-dos-iguales? lista)
  (and (longitud-mas-2? lista)
       (cuantos-hay lista (first lista))
       (= 1 (- (get-tam lista) (get-tam (elimina-dup lista))))
       ))

; (solo-dos-iguales? '()) ; ⇒ #f
; (solo-dos-iguales? '(a)) ; ⇒ #f
; (solo-dos-iguales? '(a b c a)) ; ⇒ #t
; (solo-dos-iguales? '(a b c b a a)) ; ⇒ #f
; (solo-dos-iguales? '(a b c a a)) ; ⇒ #f
; (solo-dos-iguales? '(a b c a b)) ; ⇒ #f

; Ejercicio 3
; a.1)
(define p1
  (list (cons 'a 'b) 'c (list 'd 'e)))

(define p1a
  (cons (cons 'a 'b) (cons 'c (cons (cons 'd (cons 'e '()) ) '()))))

(caja-puntero p1a)

; a.2)
(cons (cdr (first p1)) (first (third p1)))

; b.1)
(define p2
  (cons (list (cons 'a (cons 'b 'c)) (list 'd 'e) 'f) (list 'g)))
(caja-puntero p2)

; b.2)
(cons (cddr (first (car p2 ))) (second (second (first p2 ))))

; Ejercicio 4
(define (data-eq? p)
  (equal? (car p) (cdr p)))

(define (contar-datos-iguales lista-parejas)
  (if (null? lista-parejas)
      0
      (if (data-eq? (first lista-parejas))
          (+ 1 (contar-datos-iguales (rest lista-parejas)))
          (+ 0 (contar-datos-iguales (rest lista-parejas)))
          )))

; (contar-datos-iguales '((2 . 3) ("hola" . "hola") (\#a . \#a) (true . false))) ; ⇒ 2
; (contar-datos-iguales '((2 . "hola") ("hola" . 3) (\#a . true) (\#b . false))) ; ⇒ 0

; Ejercicio 5
;;;; IMPORTADO DE LA PRÁCTICA 2
(define (obten-valor char)
  (cond
    ((equal? char #\A) 1)
    ((equal? char #\J) 10)
    ((equal? char #\Q) 11)
    ((equal? char #\K) 12)
    (else (- (char->integer char) (char->integer #\0)))))

(define (obten-palo char)
  (cond
    ((equal? char #\♠) 'Picas)
    ((equal? char #\♣) 'Tréboles)
    ((equal? char #\♥) 'Corazones)
    ((equal? char #\♦) 'Diamantes)))

(define (cs->p s)
  (cons (string-ref (symbol->string s) 0) (string-ref (symbol->string s) 1) ))

(define (carta c)
  (cons (obten-valor (car (cs->p c))) (obten-palo (cdr (cs->p c)))))

(define (valor-carta c)
  (car (carta c)))
;;;; FIN IMPORTE

(define mano1 '(A♦ 2♦ 3♣ 4♦ 5♥))
(define mano2 '(J♦ J♣ J♠ J♥ K♣))

; a
(define (palo-carta c)
  (cdr (carta c)))

; (palo-carta 'A♠) ; ⇒ Picas
; (palo-carta '2♣) ; ⇒ Tréboles
; (palo-carta '3♥) ; ⇒ Corazones
; (palo-carta '4♦) ; ⇒ Diamantes

; b
(define (veces-palo l p)
  (if (null? l)
      0
      (if (equal? (palo-carta (first l)) p)
          (+ 1 (veces-palo (rest l) p))
          (veces-palo (rest l) p))))

; (veces-palo '(5♠ 6♣ 7♥ 8♦ 9♠) 'Picas) ; ⇒ 2
; (veces-palo '(J♠ Q♣ K♥) 'Diamantes) ; ⇒ 0
; (veces-palo '(A♣ 2♥ 3♠) 'Corazones) ; ⇒ 1
; (veces-palo '() 'Tréboles) ; ⇒ 0

(define (color-equal-len l)
  (veces-palo l (palo-carta (first l))))

(define (color? l)
  (and (not (null? l))
       (= (color-equal-len l) (length l))))

; (palo-carta (first '(5♣ J♦ J♣ Q♠ Q♥)))
; (color? '(5♣ J♦ J♣ Q♠ Q♥)) ; ⇒ #f
; (color? '(2♦ 5♦ 6♦ J♦ K♦)) ; ⇒ #t

; c

(define (escalera? l)
  (and (not (null? l))
       (not (null? (rest l)))
       (< (valor-carta (first l))
          (valor-carta (first (rest l))))))

; (escalera? '(5♣ 4♦ 3♣)) ; ⇒ #f
; (escalera? '(8♣ 9♦ J♣ Q♦)) ; ⇒ #t
; (escalera? '(8♣ 2♣)) ; ⇒ #f
; (escalera? '(A♣ 2♦ 3♣)) ; ⇒ #t
; (escalera? '(A♣)) ; ⇒ #t
; (escalera? '()) ; ⇒ #t

; Ejercicio 6
; a

(define (suma-izq c a)
  (cons (+ (car c) a) (cdr c)))

(define (suma-der c a)
  (cons (car c) (+ (cdr c) a)))

; (suma-izq (cons 10 20) 3)  ; ⇒ (13 . 20)
; (suma-der (cons 10 20) 5)  ; ⇒ (10 . 25)

; b.1)

(define (recursivo-sum x)
  (suma-impares-pares (rest x)))

(define (recursivo-der r)
  (suma-der (recursivo-sum r) (first r)))

(define (recursivo-izq l)
  (suma-izq (recursivo-sum l) (first l)))

(define (suma-impares-pares l)
  (cond
    ((null? l) (cons 0 0))
    ((even? (first l)) (recursivo-der l))
    ((odd? (first l)) (recursivo-izq l))))

; (suma-impares-pares '(3 2 1 4 8 7 6 5)) ; ⇒ (16 . 20)
; (suma-impares-pares '(3 1 5))           ; ⇒ (9 . 0)

; b.2)
; Dada la siguiente llamada, indica qué devuelve la primera llamada recursiva:
; (suma-impares-pares '(2 1 2 1 4))
; > La primera llamada recursiva devolvería:
;   (suma-impares-pares '(2 1 2 1 4))
;   (suma-der (recursivo-sum '(2 1 2 1 4)) (first '(2 1 2 1 4))))
;   (suma-der (recursivo-sum '(1 2 1 4)) 2)
;   que posteriormente es:
;   (suma-der (cons 2 6) 2)

; c

(define (hay-mas? l)
  (null? l))

(define (format-pair-str6 elem)
  (cons elem (string-length elem)))

(define (recursivo-str d)
  (> (string-length (first d)) (cdr (cadena-mayor (rest d)))))

(define (cadena-mayor lista)
  (if (hay-mas? lista)
      (format-pair-str6 "")
      (format-pair-str6 (if (recursivo-str lista)
                            (first lista)
                            (car (cadena-mayor (rest lista)))
                            ))))
          

; (cadena-mayor '()) ; ⇒ ("" . 0)
; (cadena-mayor '("vamos" "a" "obtener" "la" "cadena" "mayor")) ; ⇒  ("obtener" . 7)
; (cadena-mayor '("prueba" "con" "maximo" "igual")) ; ⇒ ("maximo" . 6)
