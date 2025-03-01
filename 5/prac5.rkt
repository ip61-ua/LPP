#lang racket
(require "lpp.rkt")

;; Ejercicio 1

;; a)

(define (aplica-veces f1 f2 n x)
  (if (= n 0)
      x
      (aplica-veces f1 f2 (- n 1) (f1 (f2 x)) )))

;(aplica-veces (lambda (x) (* 2 x)) (lambda (x) (+ 2 x)) 3 5) ; ⇒ 68
;(aplica-veces (lambda (x) (+ x 1)) (lambda (x) (+ x 2)) 2 10) ; ⇒ 16
;(aplica-veces (lambda (x) (* x x)) (lambda (x) (+ x 1)) 4 3) ; ⇒ 7072978201

;; b)

(define (intercambia-dos-primeros pred l)
  (if (pred (second l))
      (cons (second l) (cons (first l) (rest (rest l))))
      l))

(define (mueve-al-principio-condicion pred lista)
  (cond
    ((or (null? lista)
          (null? (rest lista))
          (pred (first lista))) lista)
    ((pred (second lista)) (intercambia-dos-primeros pred lista))
    (else (intercambia-dos-primeros pred (cons (first lista)
                                                (mueve-al-principio-condicion pred (rest lista)))))))

;(mueve-al-principio-condicion number? '(a b)) ; ⇒ '(a b)
;(mueve-al-principio-condicion number? '(a b c 1 d 1 e)) ; ⇒ (1 a b c d 1 e)
;(mueve-al-principio-condicion number? '(1 a b 1 c)) ; ⇒ (1 a b 1 c)
;(mueve-al-principio-condicion number? '(a b c d)) ; ⇒ '(a b c d)

;; c)

(define (aux-check p a b)
  (if (p (first a) (first b))
      (cons (first a) (first b))
      '()))

(define (elimina-vacio par)
  (if (null? (first par))
      (rest par)
      par))

(define (comprueba pred lista1 lista2)
  (if (or (null? lista1) (null? lista2))
      '()
      (elimina-vacio (cons (aux-check pred lista1 lista2)
                           (comprueba pred (rest lista1) (rest lista2))))))

;(comprueba (lambda (x y)
;             (= (string-length (symbol->string x)) y))
;           '(este es un ejercicio de examen) 
;           '(2 1 2 9 1 6))
; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))

;(comprueba (lambda (x y)
;              (= (string-length x) (string-length y)))
;             '("aui" "a" "ae" "c" "aeiou")
;             '("hola" "b" "es" "que" "cinco"))
; ⇒ (("a" . "b") ("ae" . "es") ("aeiou" . "cinco"))

;; Ejercicio 2

;; a)

(define (inserta-ordenada n lista-ordenada)
  (cond
    ((null? lista-ordenada) (list n))
    ((<= n (first lista-ordenada)) (cons n lista-ordenada))
    (else (cons (first lista-ordenada)
                (inserta-ordenada n (rest lista-ordenada))))))

; (inserta-ordenada 10 '()) ;-> '(10)
; (inserta-ordenada 10 '(11)) ;-> '(10 11)
; (inserta-ordenada 10 '(1)) ;-> '(1 10)
; (inserta-ordenada 10 '(-8 2 3 11 20)) ;-> '(-8 2 3 10 11 20)

(define (inserta-ordenada-genérica n lista-ordenada menor-igual?)
  (cond
    ((null? lista-ordenada) (cons n lista-ordenada))
    ((menor-igual? n (first lista-ordenada)) (cons n lista-ordenada))
    (else (cons (first lista-ordenada) (inserta-ordenada-genérica n (rest lista-ordenada) menor-igual?)))))

; (inserta-ordenada-genérica 10 '() <=) ;-> '(10)
; (inserta-ordenada-genérica 10 '(11) <=) ;-> '(10 11)
; (inserta-ordenada-genérica 10 '(1) <=) ;-> '(1 10)
; (inserta-ordenada-genérica 10 '(-8 2 3 11 20) <=) ;-> '(-8 2 3 10 11 20)

; (ordena '(2 -1 100 4 -6)) ;-> '(-6 -1 2 4 100)

(define (ordena-genérica lista menor-igual?)
  (if (null? lista)
      '()
      (inserta-ordenada-genérica (first lista) (ordena-genérica (rest lista) menor-igual?) menor-igual?)))

; (ordena-genérica '(2 -1 100 4 -6) <=) ;-> '(-6 -1 2 4 100)
; (ordena-genérica '() <=) ;-> '()
; (ordena-genérica '(1) <=) ;-> '(1)
; (ordena-genérica '(2 1) <=) ;-> '(1 2)

;; b)

;(ordena-genérica '("Hola" "me" "llamo" "Iñigo" "Montoya") (lambda (a b) (<= (string-length a) (string-length b)) )) ; -> '("me" "Hola" "llamo" "Iñigo" "Montoya"))
;(ordena-genérica '("Hola" "me" "llamo" "Iñigo" "Montoya") string<? ) ; -> '("Hola" "Iñigo" "Montoya" "llamo" "me"))
;(ordena-genérica '((2 . 2) (1 . 1) (3 . 0) (5 . 1)) (lambda (a b) (< (+ (car a) (cdr a)) (+ (car b) (cdr b))) ) ) ; -> '((1 . 1) (3 . 0) (2 . 2) (5 . 1)))


;; c)

;; Cada vez que veo el ejercicio de cartas me entran ganas de cometer algún crimen.
;; #NoMásExesDeCartas #NoEsFuncional #FelizLunes #MociónDeCensura

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

(define (ordena-cartas l)
  (ordena-genérica l (lambda (a b) (< (valor-carta a) (valor-carta b)))))

;(ordena-cartas '(Q♠ J♣ 5♣ Q♥ J♦)) ; ⇒ '(5♣ J♣ J♦ Q♠ Q♥)

;; Ejercicio 3

;; a)

; FOS. Esto es Javascript

;(map (lambda (x)
;         (cond 
;            ((symbol? x) (symbol->string x))
;            ((number? x) (number->string x))
;            ((boolean? x) (if x "#t" "#f"))
;            (else "desconocido"))) '(1 #t hola #f (1 . 2))) ; ⇒ ("1" "#t" "hola" "#f" "desconocido") ;; Importante lo devuelve sin quote

;(filter (lambda (x) (equal? (string-ref (symbol->string x) 1) #\a))
;        '(alicante barcelona madrid almería)) ; ⇒ (barcelona madrid) ;; Fíjate que lo hace sin quote

;(foldr (lambda (dato resultado) (string-append dato "*" resultado)) ;; <---- foldr
;       "" ; <ESTO>                      ;; <ESTO> es el valor-acumulador e inicializador. Si la lista <PARAM 2> es vacía, solo devuelve <ESTO>. En la 1a exec, resultado valdría <ESTO>
;       '("Hola")) ; ⇒ "Hola*que*tal*"  ;; Para foldr: una lista, para el lambda de dentro una variable (un elem de la lista)

;; Importante: (cons '(9) '(1)) => ((9) 1)             append => lists + lista
;;             (cons 9 '(1)) => (9 1)                  cons => elemento + lista
;;             (append '(9) 1) => (9 . 1)   => pareja
;;             (append '(9) '(1)) => (9 1)  => lista
;(append '(9) '(1))

;(foldr append '() '((1 2) (3 4 5) (6 7) (8))) ; ⇒ (1 2 3 4 5 6 7 8)

;(foldl (lambda (dato resultado)
;        (string-append
;          (symbol->string (car dato))
;          (symbol->string (cdr dato))
;          resultado)) "" '((a . b) (hola . adios) (una . pareja))) ; ⇒ "unaparejaholaadiosab"

;(foldr (lambda (dato resultado)
;           (cons (+ (car resultado) dato)
;                 (+ (cdr resultado) 1))) '(0 . 0) '(1 1 2 2 3 3)) ; ⇒ (12 . 6)

;(apply + (map cdr '((1 . 3) (2 . 8) (2 . 4)))) ; ⇒ 15

;(apply min
;       (map car
;           (filter (lambda (p) (> (car p) (cdr p))) 
;                   '((3 . 1) (1 . 20) (5 . 2))))) ; ⇒ ((3 . 1) (5 . 2)) => (3 5) => 3

;; b)

; Los siguientes ejercicios utilizan esta definición de lista

(define lista '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))


; Queremos obtener una lista donde cada número es la suma de las
; parejas que son pares

;(filter (lambda (x) (= (modulo x 2) 0))
;        (map (lambda (x) (+ (car x)
;                                 (cdr x)))
;               lista))
; ⇒ (8 14 10)

; Queremos obtener una lista de parejas invertidas donde la "nueva"
; parte izquierda es mayor que la derecha.

;(filter (lambda (c) (> (car c) (cdr c)))
;        (map (lambda (c) (cons (cdr c) (car c))) lista))
; ⇒ ((7 . 2) (5 . 3))

; Queremos obtener una lista cuyos elementos son las partes izquierda
; de aquellas parejas cuya suma sea par.
;(filter (lambda (x) (even? (+ (car x) (cdr x)))) lista)

;(foldr (lambda (cur acc) (cons (car cur) acc))
;       '()
;       (filter (lambda (x) (even? (+ (car x) (cdr x)))) lista)
;       )
; ⇒ (3 10 5)

; IMPORTANTE folr (cur, acc)

;; c)

(define (f1 x) (lambda (y z) (string-append y z x)))
(define g1 (f1 "a"))
; (g1 "clase" "lpp") ; => "claselppa"



(define (f2 x) (lambda (y z) (list y x z)))
(define g2 (f2 "lpp"))
;(g2 "hola" "clase") ; => (list "hola" "lpp" "clase")


(define (f3 g3) (lambda(z x) (g3 z x)))
;( (lambda (a b) ((f3 cons) a b)) 3 4 )
; => '(3 . 4)

;; Ejercicio 4

;; a)

(define (contar-datos-iguales-fos l)
  (length (filter (lambda (c) (equal? (car c) (cdr c)) ) l) ))

;(contar-datos-iguales-fos 
;   '((2 . 3) ("hola" . "hola") (\#a . \#a) (true . false))) 
; ⇒ 2
; 
;   '((2 . "hola") ("hola" . 3) (\#a . true) (\#b . false))) 
; ⇒ 0

;; b)

(define (expande-pareja l)
  (cond
    ((null? l) '())
    ((= 0 (cdr l)) '())
    (else (cons (car l) (expande-pareja (cons (car l) (- (cdr l) 1) )  )) )
    ))

(define (expande-lista-fos l)
  (map expande-pareja l))

(expande-lista-fos '((#t . 3) ("LPP" . 2) (b . 4))) 
; ⇒ '(#t #t #t "LPP" "LPP" b b b b))

