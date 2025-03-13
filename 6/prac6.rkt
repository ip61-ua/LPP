#lang racket
(require "lpp.rkt")

; 1a
(define (concat lista)
  (concat-iter lista ""))

(define (concat-iter l acc)
  (if (null? l)
      acc
      (concat-iter (rest l) (string-append acc (first l)))))
  
;(concat  '("hola" "y" "adiós")) ; ⇒ "holayadiós"
;(concat-iter '("hola" "y" "adiós") "") ; ⇒ "holayadiós"

; 1b
(define (min-max lista)
  (min-max-iter lista (cons 0 0)))

(define (min-max-iter l acc)
  (cond
    ((null? l) acc)
    ((< (first l) (car acc)) (min-max-iter (rest l) (cons (first l) (cdr acc))))
    ((> (first l) (cdr acc)) (min-max-iter (rest l) (cons (car acc) (first l))))
    (else (min-max-iter (rest l) acc)))
    )

;(min-max '(2 5 9 12 5 0 4)) ; ⇒ (0 . 12)
;(min-max '(3 2 -8 4 10 0))  ; ⇒ (-8 . 10)
;(min-max-iter '(5 9 12 -2 5 0 4) (cons 2 2)) ; ⇒ (-2 . 12)

; 2a
(define (expande-pareja-iter p acc)
  (if (or (null? p) (= (cdr p) 0))
      acc
      (expande-pareja-iter (cons (car p) (- (cdr p) 1)) (append acc (list (car p))))))

(define (expande-pareja p)
  (expande-pareja-iter p '()))

(define (expande-parejas-iter l acc)
  (if (null? l)
      acc
      (expande-parejas-iter (rest l) (append acc (expande-pareja-iter (first l) '())))
      ))

(define (expande-parejas . l)
  (expande-parejas-iter l '()))

;(expande-pareja (cons 'a 4)) ; ⇒ (a a a a)
;(expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4)) ; ⇒ (#t #t #t "LPP" "LPP" b b b b)

; 2b
(define (rotar n l)
  (if (or (= 0 n) (null? l))
      l
      (rotar (- n 1) (append (list-tail l 1) (list (first l))))))

(rotar 4 '(a b c d e f g)) ; ⇒ (e f g a b c d)