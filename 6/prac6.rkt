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

;(rotar 4 '(a b c d e f g)) ; ⇒ (e f g a b c d)

; 3a
(define (mi-foldl f i l)
  (if (null? l)
      i
      (mi-foldl f (f (first l) i) (rest l))))

;(mi-foldl string-append "****" '("hola" "que" "tal")) ; ⇒ "talquehola****"
;(mi-foldl cons '() '(1 2 3 4)) ; ⇒ (4 3 2 1)

; 3b
(define (binario-a-decimal-iter l acc)
  (if (null? l)
      acc
      (binario-a-decimal-iter (rest l) (+ (* 2 acc) (first l)))))

(define (binario-a-decimal l)
  (binario-a-decimal-iter l 0))

;(binario-a-decimal '(1 1 1 1)) ; ⇒ 15
;(binario-a-decimal '(1 1 0)) ; ⇒ 6
;(binario-a-decimal '(1 0)) ; ⇒ 2

;4
(define mi-dic (make-dic))
(put 1 10 mi-dic) ; ⇒ 10
(get 1 mi-dic) ; ⇒ 10
(key-exists? 2 mi-dic) ; ⇒ #f

(define (store where d what)
  (put where what d))

(define (memo-or-pascal f c d)
  (if (key-exists? (cons f c) d)
      (get (cons f c) d)
      (put (cons f c) (pascal-memo f c d) d)
      ))

(define (pascal-memo fila col d)
   (cond ((= col 0) 1)
         ((= col fila) 1)
         (else (+ (memo-or-pascal (- fila 1) (- col 1) d)
                  (memo-or-pascal (- fila 1) col d) ))))

(define diccionario (make-dic))
;(pascal-memo 8 4 diccionario) ; ⇒ 70
;(pascal-memo 40 20 diccionario) ; ⇒ 137846528820

; 5a
(require 2htdp/image)

(define (kline t) (line t 0 "black"))

(define (koch nivel trazo)
  (if (< 0 nivel) 
      (beside/align "bottom"
                    (koch (- nivel 1) trazo)
                    (rotate 60 (koch (- nivel 1) trazo))
                    (rotate -60 (koch (- nivel 1) trazo))
                    (koch (- nivel 1) trazo))
      (kline trazo)))

(koch 6 0.01)



