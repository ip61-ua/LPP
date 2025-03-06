#lang racket

(require "lpp.rkt")

;;;; 2017-2028

;;;; Exe 1

;;;; a.1) b
;;;; a.2) a
;;;; a.3) ?
;;;; a.4) c
;;;; a.5) b

;;;; Exe 2
;;;; a)
;;;; a.1)
(define p (cons 1 (cons (list (cons 2 3) 4) (cons 5 6))))
;(caja-puntero p)
;;;; a.2)
;(cadadr p)

;;;; b)
(define (generica-rec fun1 fun2 lista base)
  (if (null? lista)
      base
      (fun1 (fun2 (car lista))
            (generica-rec fun1 fun2 (cdr lista) base))))

(define (generica-fos fun1 fun2 lista base)
  (foldl (lambda (cur acc) (fun1 (fun2 cur) acc))
         base
         lista))

;(generica-rec + string-length '("hola" "adios") 0)
;(generica-fos + string-length '("hola" "adios") 0)

;;;; c)
(define (f x)
  (lambda (y)
    (+ x y)))
;((f 3) 3); => 6

(define (g x)
  (lambda (y)
    (- x y)))
;((g 6) 3); => 3

;;;; d)

;(foldl (lambda (cur acc) (cons (cons (cdr cur) (car cur)) acc)) '() '((1 . 20) (5 . 8) (7 . 10)))
; => '((10 . 7) (8 . 5) (20 . 1))

;;;; 3
;;;; a)
(define (cmp-dos-cero a b)
  (or (and (>= a 0) (<= b 0))
      (and (<= a 0) (>= b 0))))

(define (cruza-cero? lista)
  (cond
    ((null? lista) #f)
    ((= 0 (car lista)) #t)
    ((null? (rest lista)) #f)
    (else (or (cmp-dos-cero (first lista) (second lista))
              (cruza-cero? (rest lista))))))

;(cruza-cero? '())
;(cruza-cero? '(1))
;(cruza-cero? '(0))  
;(cruza-cero? '(-10 -5 -2 10 20)) ;⇒ #t
;(cruza-cero? '(-20 -12 -9 -3)) ;⇒ #f
;(cruza-cero? '(3 12 18 20 25)) ;⇒ #f

;;;; b)
(define (restagrama l)
  (if (null? l)
      '()
      (cons l (restagrama (rest l)))))

;(restagrama '(c h o l a)) ; => '((c h o l a) (h o l a) (o l a) (l a) (a)))
;(restagrama '(u n o)) ; => '((u n o) (n o) (o)))

;;;; Exe 4

;;;; a)
(define (crea-lista n elem)
  (if (= n 0)
      '()
      (cons elem (crea-lista (- n 1) elem))))

;(crea-lista 3 #\o)
;(crea-lista 5 2)

;;;; b)
(define (expande-simbolos-recursivo l)
  (append (crea-lista (car (first l)) (cdr (first l)))
          (expande-simbolos (rest l))))

(define (expande-simbolos lista-parejas)
  (cond
    ( (null? lista-parejas) '() )
    ( (symbol? (cdr (first lista-parejas))) (expande-simbolos-recursivo lista-parejas))
    ( else (expande-simbolos (rest lista-parejas)))
    ))


;(expande-simbolos '((2 . a) (4 . #\a) (3 . b) (5 . #f)))

;;;; c)
(define (concatena lista)
  (foldr append '() lista))

(define (simbolos-filtro l)
  (filter (lambda (x) (symbol? (cdr x))) l))

(define (cb-expande x)
  (crea-lista (car x) (cdr x)))

(define (expande-simbolos-fos lista-parejas)
  (concatena (map cb-expande
                  (simbolos-filtro lista-parejas))))

;(expande-simbolos-fos '((2 . a) (4 . #\a) (3 . b) (5 . #f)))

;;;; Exe 5

;;;; a)
(require rackunit)

(define (añade-izq elem pareja-listas)
  (cons (cons elem (car pareja-listas) ) (cdr pareja-listas) ))

(define (añade-der elem pareja-listas)
  (cons (car pareja-listas) (cons elem (cdr pareja-listas))))

(check-equal? (añade-izq 'a (cons '(b c) '(d))) '((a b c) . (d)))
(check-equal? (añade-der 'a (cons '(b c) '(d))) '((b c) . (a d)))

;;;; b)

(define (es-primero-par? l)
  (= 0 (remainder (first l) 2)))

(define (insertar l mode)
  (mode (first l) (separar-pares-impares (rest l))))

(define (es-primero-impar? l)
  (not (es-primero-par? l)))

(define (separar-pares-impares lista-num)
  (cond
    ( (null? lista-num) (cons '() '()))
    ( (es-primero-par? lista-num) (insertar lista-num añade-izq))
    ( (es-primero-impar? lista-num) (insertar lista-num añade-der))))

;(separar-pares-impares '(3 6 8 1 5 4)) ; ⇒ {{6 8 4} . {3 1 5}}

;;;; c) 
(define (get-insert-mode x)
  (if (es-primero-par? (list x)) añade-izq añade-der))

(define (separar-pares-impares-fos lista-num)
  (foldr (lambda (cur acc) ((get-insert-mode cur) cur acc))
         (cons '() '())
         lista-num))

(separar-pares-impares-fos '(3 6 8 1 5 4)) ; ⇒ {{6 8 4} . {3 1 5}}