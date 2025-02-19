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

;(comprueba-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) ; ⇒ ((un . 2) (ejercicio . 9) (examen . 6))

;; Ejercicio 4

;; a)

(define (do-nextpair p)
  (expande-pareja (cons (car p) (- (cdr p) 1))))

(define (expande-pareja p)
  (if (or (null? p) (= (cdr p) 0))
      '()
      (append (list (car p)) (do-nextpair p))))

;(expande-pareja '(hola . 3)) ; ⇒ (hola hola hola)
;(expande-pareja '(#t . 5)) ; ⇒ (#t #t #t #t #t)

;; b)

(define (expande-parejas . pareja_n)
  (if (null? pareja_n)
      '()
      (append (expande-pareja (first pareja_n)) (apply expande-parejas (rest pareja_n)))))
 

;(expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4)) ; ⇒ (#t #t #t "LPP" "LPP" b b b b)

;; b.1)

;; hecho.

;; b.2)

;; hecho.

;; c)

(define (first-not-num? l)
  (not (number? (first l))))

(define (first-num? l)
  (number? (first l)))

(define (parse-pareja p)
  (expande-pareja (cons (second p) (first p))))

(define (expnd-recursivo p l)
   (append p (expande (rest l))))

(define (expnd-not-num-r l)
  (expnd-recursivo (list (first l)) l))

(define (expnd-num-r l)
  (expnd-recursivo (parse-pareja l) (rest l)))

(define (expande lista)
  (cond
    ((null? lista) '())
    ((first-not-num? lista) (expnd-not-num-r lista))
    ((first-num? lista) (expnd-num-r lista))))

;(expande '(4 clase ua 3 lpp aulario)) ; ⇒ (clase clase clase clase ua lpp lpp lpp aulario)

;; Ejercicio 5

;; a)

;((lambda (x) (* x x)) 3) ; ⇒ 9
;((lambda () (+ 6 4))) ; ⇒ 10
;((lambda (x y) (* x (+ 2 y))) (+ 2 3) 4) ; ⇒ 30
;((lambda (x y) (* x (+ 2 x))) 5) ; ⇒ ERROR DE ARGUMENTOS (arity mismatch;)

(define f (lambda (a b) (string-append "***" a b "***")))
(define g f)
;(procedure? g) ; ⇒ #t
;(g "Hola" "Adios") ; ⇒ "***HolaAdios***"

;; b)

(define suma-3 (lambda (x) (+ x 3)))

(define factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))

;; c)

(define (doble x)
   (* 2 x))

(define (foo f g x y)
   (f (g x) y))

;(foo + 10 doble 15) ; ⇒ ERROR
;  (+ (10 doble) y)) ; el número 10 no es procedimiento

;(foo doble + 10 15) ; ⇒ ERROR
;  (doble (g x) y) ; Arity mismatch. doble solo acepta un argumento.

;(foo + doble 10 15) ; ⇒ 35
;  (+ (doble 10) 15)
;  (+ 20 15)
;  35

;(foo string-append (lambda (x) (string-append "***" x)) "Hola" "Adios") ; ⇒ "***HolaAdios"
;  (string-append (lambda... "Hola") "Adios"))
;  (string-append "***Hola" "Adios")
;  "***HolaAdios"

(define (bar f p x y)
   (if (and (p x) (p y))
       (f x y)
       'error))

;(bar doble number? 10 15) ; ⇒ ERROR
; (if (and (number? 10) (number? 15)) (doble 10 15) 'error))
; (doble 10 15) ; Arity mismatch

;(bar string-append string? "Hola" "Adios") ; ⇒ "HolaAdios"
; (if (and (string? "Hola") (string? "Adios")) (string-append "Hola" "Adios") 'error))
; (string-append "Hola" "Adios")
; "HolaAdios"

;(bar + number? "Hola" 5) ; ⇒ 'error
; (if (and (number? "Hola") (number? 5)) (f x y) 'error))
; 'error

;; Ejercicio 6

;; a)

(require "lpp.rkt")

(define (junt l a)
  (append (list a) l))

(define (putl pos l obj)
   (if (null? l)
       (list obj)
       (junt (pos l) obj)))

(define (coloca tres-listas un dos tres)
  (list (putl first tres-listas un)
        (putl second tres-listas dos)
        (putl third tres-listas tres)))

;(coloca null 'a 'b 'c) ; ⇒ '((a) (b) (c))
;(coloca '(() () ()) 'a 'b 'c) ; ⇒ '((a) (b) (c))
;(coloca '((a) (a) (a)) 'b 'b 'b) ; ⇒ '((b a) (b a) (b a))
;(coloca '((a) (b c) (d e f)) 'g 'h 'i) ; ⇒ '((g a) (h b c) (i d e f)))

;; b)

(define (reparte-tres lista-cartas)
  (if (null? lista-cartas)
      '()
      (coloca (reparte-tres (rest (rest (rest lista-cartas))))
              (first lista-cartas)
              (second lista-cartas)
              (third lista-cartas))))

(define doce-cartas '(A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ J♣ Q♣ K♣))
(reparte-tres doce-cartas) ; ⇒ '((A♣ 4♣ 7♣ J♣) (2♣ 5♣ 8♣ Q♣) (3♣ 6♣ 9♣ K♣))

;; c)

(define (quita-ultimo l)
  (if (null? (rest l))
      '()
      (append (list (first l)) (quita-ultimo (rest l)))
      ))

(define (elemento-central l)
  (if (null? (rest l))
      (first l)
      (elemento-central (quita-ultimo (rest l)))
      ))

(elemento-central '(a b c d e f g)) ; ⇒ d