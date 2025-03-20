#lang racket
(require "lpp.rkt")
(require rackunit)

; Ejercicio 1
; a
(define lista-a '((a b) d (c (e) (f g) h)))
(check-equal? (fourth (third lista-a)) 'h)
;(pinta-lista lista-a)

; b
(define lista-b1 '((2 (3)) (4 2) ((2) 3)))
; L1        *
; L2  *     *     *
; L3 2 *   4 2   * 3
; L4   3         2
(define lista-b2 '((b) (c (a)) d (a)))
; L1         *
; L2 *    *    d   *
; L3 b   c *       a
; L4       a
(pinta-lista lista-b2)

; c
(define (cuadrado-estruct lista)
  (cond ((null? lista) '())
        ((hoja? lista) (* lista lista ))
        (else (cons
               (cuadrado-estruct (first lista))    ; El argumento aquí puede ser una hoja o lista
               (cuadrado-estruct (rest lista)))))) ; Aquí siempre es una lista

;(pinta-lista (cuadrado-estruct lista-b1))
; 1 llamada
; first: (cuadrado-estruct (first lista)) ; (2 (3))
; rest:  (cuadrado-estruct (rest lista))  ; ((4 2) ((2) 3))
; 2 llamada
; first  2         ; devolver * 2 2
; rest   ((3))
; first  (4 2)
; rest   (((2) 3))
; 3 llamada
; first (3)
; rest  ()         ; devolver ()
; first 4          ; devolver * 4 4
; rest  (2)
; first ((2) 3)
; rest  ()         ; devolver ()
; 4 llamada
; first 3          ; devolver * 3 3
; rest  ()         ; devolver ()
; first 2          ; devolver * 2 2
; rest  ()         ; devolver ()
; first (2)
; rest  (3)
; 5 llamada
; first 2          ; devolver * 2 2
; rest  ()         ; devolver ()
; first 3          ; devolver * 3 3
; rest  ()         ; devolver ()

; d
(define (suma-1-si-mayor-igual-que-0 x)
  (if (>= x 0)
      (+ x 1)
      x))

(define (nivel-hoja-fos dato ld)
  (if (hoja? ld)
      (if (equal? ld dato)
          0
          -1)
      (suma-1-si-mayor-igual-que-0 (foldr max
                                          -1
                                          (map (lambda (elem) (nivel-hoja-fos dato elem))
                                               ld)))))

;(map (lambda (elem) (nivel-hoja-fos 'a elem)) lista-b2) ; (-1 2 -1 1)

; Ejercicio 2
; a

; recursivo puro
(define (concatena-recursivo a l)
  (string-append a (concatena (rest l))))

(define (concatena l)
  (cond
    ((null? l) "")
    ((hoja? (first l)) (concatena-recursivo (symbol->string (first l)) l))
    (else (concatena-recursivo (concatena (first l)) l))))

; con fos
(define (concatena-fos l)
  (foldr (lambda (cur acc)
           (if (hoja? cur)
               (string-append (symbol->string cur) acc)
               (string-append (concatena-fos cur) acc)))
         ""
         l))

;(concatena-fos '(a b (c) d)) ; ⇒ "abcd"
;(concatena-fos '(a (((b)) (c (d (e f (g))) h)) i)) ; ⇒ "abcdefghi"

; b
(define (todos-positivos? l)
  (or (null? l)
      (if (hoja? (first l))
          (and (>= (first l) 0) (todos-positivos? (rest l)))
          (and (todos-positivos? (first l)) (todos-positivos? (rest l))))))

(define (todos-positivos-fos-aux? x)
  (if (hoja? x)
      (>= x 0)
      (todos-positivos-fos? x)))

(define (todos-positivos-fos? l)
  (foldr (lambda (cur acc) (and acc (todos-positivos-fos-aux? cur)))
         #t
         l))

;(todos-positivos-fos? '(1 (2 (3 (3))) 4)) ; ⇒ #t
;(todos-positivos-fos? '(1 (2 (3 (-3))) 4)) ; ⇒ #f
;(todos-positivos-fos? '(1 (2 (3 (99999999))) 4)) ; ⇒ #t
;(todos-positivos-fos? '()) ; ⇒ #t
;(todos-positivos-fos? '(()())) ; ⇒ #t
;(todos-positivos-fos? '(()(-1))) ; ⇒ #f

; Ejercicio 3
(define (cumplen-predicado pred lista)
  (cond
    ((null? lista) '())
    ((not (hoja? (first lista))) (append (cumplen-predicado pred             ;ojito a este append
                                                            (first lista))
                                         (cumplen-predicado pred
                                                            (rest lista))))
    ((pred (first lista)) (cons (first lista)
                                  (cumplen-predicado pred
                                                   (rest lista))))
    (else (cumplen-predicado pred
                             (rest lista)))))


(define (cumplen-predicado-fos pred lista)
  (foldr (lambda (cur acc)
           (cond
             ((null? cur) acc)
             ((not (hoja? cur)) (append (cumplen-predicado-fos pred cur) acc))
             ((pred cur) (append (list cur) acc))
             (else acc)))
         '()
         lista))

;(cumplen-predicado even? '(1 2 4 6)) ; ⇒ (2 4 6)
;(cumplen-predicado even? '(1 (2 (3 (4))) (5 6))) ; ⇒ (2 4 6)
;(cumplen-predicado pair? '(((1 . 2) 3 (4 . 3) 5) 6)) ; ⇒ ((1 . 2) (4 . 3))

;(cumplen-predicado-fos even? '(1 2 4 6)) ; ⇒ (2 4 6)
;(cumplen-predicado-fos even? '(1 (2 (3 (4))) (5 6))) ; ⇒ (2 4 6)
;(cumplen-predicado-fos pair? '(((1 . 2) 3 (4 . 3) 5) 6)) ; ⇒ ((1 . 2) (4 . 3))

;(foldr (lambda (cur acc) (append acc (list cur))) '() '(1 2 3 4 5 6))
;(append (append '() (list 6)) (list 5))

(define (busca-mayores n lista-num)
  (cumplen-predicado (lambda (x) (> x 10)) lista-num))
;(busca-mayores 10 '(-1 (20 (10 12) (30 (25 (15)))))) ; ⇒ (20 12 30 25 15)

(define (empieza-por c l)
  (cumplen-predicado-fos (lambda (x) (equal? (string-ref (symbol->string x) 0) c)) l))
;(empieza-por #\m '((hace (mucho tiempo)) (en) (una galaxia ((muy  muy) lejana)))); ⇒ (mucho muy muy)

