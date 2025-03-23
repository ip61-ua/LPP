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
;(pinta-lista lista-b2)

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

; Ejercicio 4
; a
(define (sustituye-elem elem-old elem-new lista)
  (if (null? lista)
      '()
      (cons (cond
              ((not (hoja? (first lista))) (sustituye-elem elem-old
                                                           elem-new
                                                           (first lista)))
              ((equal? (first lista) elem-old) elem-new)
              (else (first lista)))
            (sustituye-elem elem-old elem-new (rest lista)))))

(define (sustituye-elem-fos elem-old elem-new lista)
  (map (lambda (x)
         (cond
           ((not (hoja? x)) (sustituye-elem-fos elem-old
                                                elem-new
                                                x))
           ((equal? x elem-old) elem-new)
           (else x)))
       lista))

;(sustituye-elem-fos 'c 'h '(c c (c))) ; ⇒ (h h (h)))
;(sustituye-elem-fos 'c 'h '(h)) ; ⇒ ()
;(sustituye-elem-fos 'c 'h '()) ; ⇒ ()
;(sustituye-elem-fos 'c 'h '(a b (c d (e c)) c (f (c) g))) ; ⇒ (a b (h d (e h)) h (f (h) g))

; b
(define (max-lvl-pair lp)
  (cond
    ((null? lp) '())
    ((null? (rest lp)) (first lp))
    ((null? (second lp)) (first lp))
    ((> (cdr (first lp)) (cdr (second lp))) (max-lvl-pair (cons (first lp) (rest (rest lp)))))
    (else (max-lvl-pair (rest lp)))))

;(max-lvl-pair '((qt . 5) (gtk . 4) (electron . -1)))
;(max-lvl-pair '((qt . 5) (gtk . 4) (electron . 99999999)))
;(max-lvl-pair '((qt . -1) (gtk . 4) (electron . -1)))

(define (nivel-mas-profundo-recursivo l)
  (cons (car l)
        (+ 1 (cdr l)))) 

(define (nivel-mas-profundo l)
  (max-lvl-pair (cons (cond
                        ((null? l) '())
                        ((not (hoja? (first l))) (nivel-mas-profundo-recursivo (nivel-mas-profundo (first l))))
                        (else (cons (first l)
                                    1)))
                      (if (null? l) '() (list (nivel-mas-profundo (rest l)))))))

(define (nivel-mas-profundo-fos l)
  (foldr (lambda (cur acc) (max-lvl-pair (list acc cur)))
         '(0 . 0)
         (map (lambda (x)
                (if (hoja? x)
                    (cons x 1)
                    (nivel-mas-profundo-recursivo (nivel-mas-profundo-fos x))))
              l)))

;(nivel-mas-profundo-fos '(2 (3))) ; ⇒ (3 . 2)
;(nivel-mas-profundo-fos '((2) (3 (4) ((((((5))) 6)) 7)) 8)) ; ⇒ (5 . 8)

; Ejercicio 5
(define (subir-lvl l)
  (foldl (lambda (cur acc)
           (if (hoja? cur)
               acc
               (append acc cur)))
         '()
         l))

(define (mezclar lista1 lista2 n)
  (map (lambda (x y)
         (cond
           ((= n 0) y)
           ((not (hoja? x)) (mezclar x y (- n 1)))
           (else x)
           ))
       lista1 lista2))

(define lista1 '(((a b) ((c))) (d) e))
(define lista2 '(((1 2) ((3))) (4) 5))
;(pinta-lista lista1)
;(pinta-lista (subir-lvl lista1))
(define lista3 (mezclar lista1 lista2 2))
;(mezclar lista1 lista2 2) ; ⇒ (((1 2) ((3))) (d) e)

; b.1
(define lista-1 '(a (b c) (d))) 
;     * 
;   / | \ 
;  a  *  *
;    / \  \ 
;   b   c  d

(define lista-2 '((e) (f) (g)))
;     * 
;   / | \ 
;  *  *  * 
; /  /    \ 
;e  f      g

(define (intersecta lista-1 lista-2)
  (append (cond ((null? lista-1) '())
                ((null? lista-2) '())
                ((and (hoja? (first lista-1))
                      (hoja? (first lista-2))) (list (cons (first lista-1) (first lista-2))))
                ((and (not (hoja? (first lista-1)))
                      (not (hoja? (first lista-2)))) (intersecta (first lista-1) (first lista-2)))
                (else '()))
          (if (or (null? lista-1)
                  (null? lista-2)) '() (intersecta (rest lista-1) (rest lista-2)) )))

(pinta-lista (intersecta lista-1 lista-2))
; ⇒ (((b . f)) ((d . g)))
;     *
;     | \
;     *  *
;    /    \
;  (b.f)  (d.g)


;(intersecta '(a b) '(c d)) ; ⇒ '((a . c) (b . d))
;(intersecta '(a (b) (c)) '(d e (f))) ; ⇒ '((a . d) ((c . f)))
