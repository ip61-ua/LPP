#lang racket

;; Ejercicio 1

3 ;; Devuelve 3
(+ 1 2 ) ;; Devuelve resultado de suma 1 y 2
(+ 1 2 3 4) ;; Devuelve resultado del sumatorio
(+) ;; OJO, devuelve 0 (caso base de suma). Para (-) (/) (sqrt) se necesita 1 argumento
(sqrt 25) ;; Devuelve 5 por ser raíz de 25. Puede usarse con decimales.
+ ;; OJO, Devuelve la función +
#\+ ;; Caracter
"+" ;; Cadena de tamaño de un char
"hola" ;; Cadena

(+ (- 4 (* 3 (/ 4 2) 4)) 3)
;; (- 4 (* 3 (/ 4 2) 4)) + 3
;; (- 4 (3*(4/2)*4))+3
;; (- 4 (48/2))+3
;; (4-24)+3 => -17

(* (+ (+ 2 3) 4) (* (* 3 3) 2))
;; "Orden aplicativo"
;; (* (+ (2+3) 4) (* (3*3) 2))
;; (* (+ ( 5 ) 4) (* ( 9 ) 2))
;; (* (5+4) (9*2))
;; 9 * 18 => 162

(* (+ 2 3 4 5) 3 (- 5 2 1))
;; "Orden normal"
;; (+ 2 3 4 5)*3*(- 5 2 1)
;; (2+3+4+5)*3*(5-2-1)

(+ (- (+ (- (+ 2 3) 5) 1) 2) 3)
;; 2+3 -5 +1 -2 +3 => 2

(- (sqrt (* 5 ( + 3 2))) (+ 1 1 1 1))
;; (sqrt (* 5 ( + 3 2))) - 4
;; sqrt(25) - 4 = 1

(> (* 3 (+ 2 (+ 3 1)) (+ 1 1)) (+ (* 2 2) 3))
;; (> (* 3 (+ 2 4) 2) (+ 4 3))
;; (> (* 3 6 2) 7)
;; #t
;; (> 3 2 3) ;; en este caso #f porque no se cumple para todos los casos
;; (> 3 2 -333.6465) ;; #t

(= (* 3 2) (+ 1 (+ 2 2) 1))
;; (= 6 6) => #t

(not (> (+ 3 2) 5))
;; (not #f) => #t

(and (even? 2) (odd? (+ 3 2)))
;; (and #t #t) => #t ;; even es par

(remainder 8 2) ;; => 0


;; Ejercicio 2

(cons 1 2) ;; pareja (1 . 2)
;; '(1 2 3) != (list 1 2 3)
;; ' no evalua => lo que hay dentro es símbolos
;; list evalúa
(car (cons 1 2)) ;; 1
;; (cons 4 (cons 2 3)) ;; (4 2 . 3) OJO
(cdr (cons 1 2))
(cons (* 2 3) (/ 4 2))
(cons (+ 2 1) (if (> 2 3) "2" "3"))
(car (car (cons (cons 1 2) 3)))
(car (cons (cons 3 4) 2)) ;; 3 . 4
(cdr (cons (cons 3 4) 2)) ;; 2
(cdr (cons 1 (cons 2 3)))
(cdr (car (cons (cons 1 2) 3))) ; 2

;; Ejercicio 3
(list 1 2 3 4)
(rest (list 1 2 3 4))
(first '(1 2 3 4))
(first (list #t 1 "Hola"))
(first (rest (list 1 2 3 4)))
(rest (rest '(1 2 3 4)))
(first (rest (rest (list 1 2 3 4))))
(list (* 2 2) (+ 1 2) (/ 4 2))
(list (+ 2 3) (- 3 4) (string-ref "hola" 3))
(cons 3 '(1 2 3))
(rest (cons #t (list "Hola" 1)))
(first (list (list 1 2) 1 2 3 4))
'((1 2) 89 7)
(list ( list 1 2) 89 7)
(cons '(1 2 3) '(4 5 6)) ;; ((1 2 3) 4 5 6)
(cons + -)
(first (rest (list 1 2 3 4)))
(rest (rest (list 1 2 3 4)))
(first (rest (rest (rest '(1 2 3 4)))))
null;

;; Ejercicio 4
(first (rest (rest (list 1 (list 2 3) (list 4 5) 6))))
;;(first (rest (rest [1 [2 3] [4 5] 6])))
;;[4 5]

(rest (rest '(1 (2 3) 4 5)))


;; Ejercicio 5
(define a 3)
(define b (+ a 1))

(if (and (> a b) (< b (* a b))) b a)

((if (< a b) + -) a b)
















;; Ejercicio 6
(define (cuadrado x)
  (* x x))

(define (diff x y)
  (- x y))

(define (diff-x p1 p2)
  (diff (car p1) (car p2)))

(define (diff-y p1 p2)
  (diff (cdr p1) (cdr p2)))

(define (suma-cuadrado p1 p2)
  (+ (cuadrado (diff-x p1 p2)) (cuadrado (diff-y p1 p2)) ))

(define (distancia p1 p2)
  (sqrt (suma-cuadrado p1 p2) ))

(distancia (cons 0 0) (cons 10 10))