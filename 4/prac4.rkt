#lang racket

;; Ejercicio 1

;; a)

(define (es-prefijo? pal1 pal2)
  (equal? pal1
          (substring pal2
                     0
                     (string-length pal1))))

(define (contiene-prefijo prefijo  lista-pal)
   (if (null? lista-pal)
      '()
       (append (list (es-prefijo? prefijo (first lista-pal)))
               (contiene-prefijo prefijo (rest lista-pal)))))

;(es-prefijo? "ane" "an--erior") ; ⇒ #f
;(es-prefijo? "ante" "anterior") ; ⇒ #t

;(contiene-prefijo "ante" '("anterior" "antígona" "antena" "anatema")) ; ⇒ (#t #f #t #f)

;; b)

(define (ambos-nulos? a b)
  (and (null? a) (null? b)))

(define (a-algo-b-null? a b)
  (and (not (null? a)) (null? b)))

(define (a-null-b-algo? a b)
  (and (null? a) (not (null? b))))

(define (a-algo-b-algo? a b)
  (and (not (null? a)) (not (null? b))))

(define (x-algo-resto x)
  (append (list (first x))
          (cadenas-mayores (rest x)
                           null)))

(define (dime-grande a b)
  (if (> (string-length a) (string-length b))
      a
      b))

(define (a-algo-rest-b-algo-resto a b)
  (append (list (dime-grande (first a) (first b)))
          (cadenas-mayores (rest a) (rest b))))

(define (cadenas-mayores a b)
  (cond
    ((ambos-nulos? a b) '())
    ((a-algo-b-null? a b) (x-algo-resto a))
    ((a-null-b-algo? a b) (x-algo-resto b))
    ((a-algo-b-algo? a b) (a-algo-rest-b-algo-resto a b))))


;(cadenas-mayores '("hola" "que" "tal") '("adios")) ; ⇒ ("adios" "que" "tal")
;(cadenas-mayores '("hola" "que" "tal") '("meme" "y" "adios")) ; ⇒ ("hola" "que" "adios")
;(cadenas-mayores '("la" "primera" "práctica" "de" "recursión")
;                 '("confiar" "en" "la" "recursión" "facilita" "su" "resolución")) ; ⇒ ("confiar" "primera" "práctica" "recursión" "recursión" "su" "resolución")

;; Ejercicios 2

;; a)

(define (inserta-pos a b c)
  (if (= b 0)
      (append (list a) c)
      (append (list (first c))
              (inserta-pos a
                           (- b 1)
                           (rest c)))))

;(inserta-pos 'b 2 '(a a a a)) ; ⇒ '(a a b a a)
;(inserta-pos 'b 0 '(a a a a)) ; ⇒ '(b a a a a)

;; b)

(define (append-principio n l)
  (append (list n) l))

(define (insorden-recursivo n l)
  (append-principio (first l)
                    (inserta-ordenada n
                                      (rest l))))

(define (inserta-ordenada n lista-ordenada)
  (cond
    ((null? lista-ordenada) (list n))
    ((< n (first lista-ordenada)) (append-principio n lista-ordenada))
    ((>= n (first lista-ordenada)) (insorden-recursivo n lista-ordenada))))

;(inserta-ordenada 10 '(-8 2 3 11 20)) ; ⇒ (-8 2 3 10 11 20)

;; c)

(define (ordena lista)
  (if (null? lista)
      '()
      (inserta-ordenada (first lista) (ordena (rest lista)))))


;(ordena '(2 -1 100 4 -6)) ; ⇒ (-6 -1 2 4 100)
;(ordena '(-1 -1 -2 -999 1 0 10)) ; ⇒ (-999 -2 -1 -1 0 1 10)

;; Ejercicio 3

;; a)

(define (es-lprimero? l d)
  (equal? (first l) d))

(define (poner-2-1 l r)
  (append (list (first r) (first l)) (rest r)))

(define (mueve-al-principio lista dato)
  (cond
    ((null? lista) '())
    ((es-lprimero? lista dato) lista)
    ((not (es-lprimero? lista dato)) (poner-2-1 lista (mueve-al-principio (rest lista) dato)))))

;(mueve-al-principio '(a b e c d e f) 'e) ; ⇒ (e a b c d e f)
;(mueve-al-principio '(a b c d e f g) 'a) ; ⇒ (a b c d e f g)
;(mueve-al-principio '(u l o z) 'z) ; ⇒ (z u l o)
;(mueve-al-principio '(a c c a) 'c) ; ⇒ (c a c a)

;; b)

(define (new-pair-symbolic s n)
  (if (= (string-length (symbol->string (first s))) (first n))
      (list (cons (first s) (first n)))
      '()))

(define (comprueba-simbolos-recursivo s n)
  (append (new-pair-symbolic s n)
          (comprueba-simbolos (rest s) (rest n))))

(define (comprueba-simbolos lista-simbolos lista-num)
  (if (null? lista-simbolos)
      '()
      (comprueba-simbolos-recursivo lista-simbolos lista-num)
      ))

(comprueba-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) ; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))
