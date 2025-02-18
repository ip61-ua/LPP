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







