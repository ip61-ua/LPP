#lang racket
(require "lpp.rkt")
(require rackunit)

(define (expande-pareja-iter pair l)
  (if (= 0 (cdr pair))
      l
      (expande-pareja-iter (cons (car pair) (- (cdr pair) 1)) (cons (car pair) l))))

(define (expande-pareja pair)
  (expande-pareja-iter pair '()))

;(expande-pareja (cons 'a 4)) ; ⇒ (a a a a)

(define (expande-parejas-iter pairs acc)
  (if (null? pairs)
      acc
      (expande-parejas-iter (rest pairs)
                            (append acc
                                    (expande-pareja (first pairs))))))

(define (expande-parejas . l)
  (expande-parejas-iter l '()))

;(expande-parejas '(#t . 3) '("LPP" . 2) '(b . 4)) ; ⇒ (#t #t #t "LPP" "LPP" b b b b)

(define (rotar num lista)
  (if (<= num 0)
      lista
      (rotar (- num 1) (append (rest lista) (list (first lista))))))

;(rotar 4 '(a b c d e f g))

(define diccionario (make-dic))

(define (pascal-memo a b dic)
  (cond
    ((or (= a b) (= a 0) (= b 0)) 1)
    ((key-exists? (cons a b) dic) (get (cons a b) dic))
    (else (+ (put (cons (- a 1) (- b 1))
                  (pascal-memo (- a 1) (- b 1) dic)
                  dic)
             (put (cons (- a 1) b)
                  (pascal-memo (- a 1) b dic)
                  dic)))))

;(pascal-memo 8 4 diccionario) ; ⇒ 70
;(pascal-memo 40 20 diccionario) ; ⇒ 137846528820

;Pascal (n, 0) = 1
;Pascal (n, n) = 1
;Pascal (fila, columna) = 
;    Pascal (fila-1,columna-1) + Pascal (fila-1, columna)

(define lista-a '((a b) d (c (e) (f g) h)))
(check-equal? (fourth (third lista-a)) 'h)


(define (concatena lista)
  (string-append (cond
                   ((null? lista) "")
                   ((hoja? (first lista)) (symbol->string (first lista)))
                   (else (concatena (first lista))))
                 (if (null? lista)
                     ""
                     (concatena (rest lista)))))

(define (concatena-fos lista)
  (foldl (lambda (cur acc)
           (string-append acc
                          (if (hoja? cur)
                              (symbol->string cur)
                              (concatena-fos cur))))
         ""
         lista))

;(concatena-fos '(a b c d)) ; ⇒ "abcd"
;(concatena-fos '(a b (c) d)) ; ⇒ "abcd"
;(concatena-fos '(a (((b)) (c (d (e f (g))) h)) i)) ; ⇒ "abcdefghi"








;;

(define lista1 '(((a b) ((c))) (d) e))
(define lista2 '(((1 2) ((3))) (4) 5))

(define (mezclar l1 l2 n)
  (map (lambda (x y)
         (cond
           ((hoja? x) x)
           ((= n 1) y)
           (else (mezclar x y (- n 1)))))
       l1 l2))

(check-equal? (mezclar lista1 lista2 2) '(((1 2) ((3))) (d) e))

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

(define (enlaza l1 l2)
  (if (null? l1) l2 (cons l1 l2)))

(define (intersecta l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cond 
        ((and (hoja? l1) (hoja? l2)) (cons l1 l2))
        ((and (not (hoja? l1))
              (not (hoja? l2))) (enlaza (intersecta (first l1) (first l2))
                                        (intersecta (rest l1) (rest l2))))
        (else '() ))
      ))

(intersecta lista-1 lista-2)
; ⇒ (((b . f)) ((d . g)))
;     *
;     | \
;     *  *
;    /    \
;  (b.f)  (d.g)

