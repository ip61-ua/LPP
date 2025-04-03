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

;(intersecta lista-1 lista-2)
; ⇒ (((b . f)) ((d . g)))
;     *
;     | \
;     *  *
;    /    \
;  (b.f)  (d.g)


(define arbol '(15 (4 (2) (3))
                   (8 (6))
                   (12 (9) (10) (11))))
(check-equal? (dato-arbol (second (rest (third (rest arbol))))) 10)

(define arbolaa (construye-arbol 15
                                 (list (construye-arbol 4 '((2) (3)))
                                       (construye-arbol 8 '((6)))
                                       (construye-arbol 12 '((9) (10) (11))))))
(check-equal? (dato-arbol (second (hijos-arbol (third (hijos-arbol arbolaa))))) 10)

;(pinta-arbol arbolaa)


(define arbol2 '(a (b (c (d)) (e)) (f)))


(define (to-string-bosque bosque)
  (if (null? bosque)
      ""
      (string-append (to-string-arbol (first bosque))
                     (to-string-bosque (rest bosque)))))

(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (to-string-bosque (hijos-arbol arbol))))

(define (to-string-arbol-fos arbol)
  (foldl (lambda (cur acc)
           (string-append acc
                          (if (hoja-arbol? cur)
                              (symbol->string (dato-arbol cur))
                              (to-string-arbol-fos cur))))
         (symbol->string (dato-arbol arbol))
         (hijos-arbol arbol)))

;(to-string-arbol-fos arbol2) ; ⇒ "abcdef"


;;
(define (hojas-cumplen pred? lista)
  (foldl (lambda (cur acc)
           (append acc (if (and (hoja-arbol? cur) (pred? (dato-arbol cur)))
                           (list (dato-arbol cur))
                           (hojas-cumplen pred? cur))))
         '()
         (hijos-arbol lista)))

(define qarbol1 '(10 (2) (12 (4) (2)) (10 (5))))
(define qarbol2 '(10 (2) (12 (4) (2)) (10 (6))))
(hojas-cumplen even? qarbol1) ; ⇒ '(2 4 2)
(hojas-cumplen even? qarbol2) ; ⇒ '(2 4 2 6)


;;

(define easwdx (construye-arbol 1
                                (list (construye-arbol 2
                                                       (list (construye-arbol 3 '((4) (2)))
                                                             (construye-arbol 5 '())))
                                      (construye-arbol 6
                                                       (list (construye-arbol 7 '()))))))

(pinta-arbol easwdx)

(define (nodos-nivel-bosque n b)
  (if (or (null? b) (< n 0))
      '()
      (append (nodos-nivel n (first b))
              (nodos-nivel-bosque n (rest b)))))

(define (nodos-nivel n t)
  (if (= 0 n)
      (list (dato-arbol t))
      (nodos-nivel-bosque (- n 1) (hijos-arbol t))))

(nodos-nivel 0 easwdx) ; ⇒ '(1)
(nodos-nivel 1 easwdx) ; ⇒ '(2 6)
(nodos-nivel 2 easwdx) ; ⇒ '(3 5 7)
(nodos-nivel 3 easwdx) ; ⇒ '(4 2)

;

(require 2htdp/image)

(define (sierpinski-elem ancho)
  (triangle ancho "solid" "olive"))

(define (componer-sierpinski figura)
  (above/align "center"
               figura
               (beside figura
                       figura)))

(define (sierpinski ancho)
  (if (< ancho 10)
      (sierpinski-elem ancho)
      (componer-sierpinski (sierpinski (/ ancho 2)))))

(sierpinski 256)

(define (koch-elem tam)
  (circle tam "outline" "blue"))

(define (componer-koch f c)
  (above (beside f f f)
         (beside f c f)
         (beside f f f)))

(define (koch tam)
  (if (< tam 10)
      (koch-elem tam)
      (componer-koch (koch (/ tam 3))
                     (circle (/ tam 3) "solid" "blue"))))


;(koch 1256)


(define (g arbol)
  (if (hoja-arbol? arbol)
      (dato-arbol arbol)
      (+ (dato-arbol arbol)
         (g (cadr (hijos-arbol arbol))))))

(define ye '(8 (3 (2)) (4 (1) (6 (1) (2))) (7) (9 (8 (3) (4)) (5))))
(define lo (construye-arbol 8
                            (list (construye-arbol 3
                                                   (list (construye-arbol 2 '())))
                                  (construye-arbol 4
                                                   (list (construye-arbol 1 '())
                                                         (construye-arbol 6
                                                                          (list (construye-arbol 1 '())
                                                                                (construye-arbol 2 '())))))
                                  (construye-arbol 7 '())
                                  (construye-arbol 9
                                                   (list (construye-arbol 8
                                                                          (list (construye-arbol 3 '())
                                                                                (construye-arbol 4 '())))
                                                         (construye-arbol 5
                                                                          '()))))))

(pinta-arbol lo)

(pinta-arbol (cadr (hijos-arbol lo)))

(g (cadr (hijos-arbol ye)))


;(define iiii '(a (b (c (e))
;                    (d))
;                 (f (g (m)
;                       (n)))
;                 (h (i)
;                    (j)
;                    (k))))

(define ca construye-arbol)

(define iiii (ca 'a (list (ca 'b (list (ca 'c (list (ca 'e '())))
                                       (ca 'd '())))
                          (ca 'f (list (ca 'g (list (ca 'm '())
                                                    (ca 'n '())))))
                          (ca 'h (list (ca 'i '())
                                       (ca 'j '())
                                       (ca 'k '()))))))

(pinta-arbol iiii)

(define (hijos-hoja? t)
  (for-all? hoja-arbol? (hijos-arbol t)))

(check-false (hijos-hoja? iiii))
(check-true (hijos-hoja? (caddr (hijos-arbol iiii))))

(define (subhijos-hoja-bosque bosque)
  (if (null? bosque) '()
      (append (subhijos-hoja (first bosque))
              (subhijos-hoja-bosque (rest bosque)))))

(define (subhijos-hoja l)
  (cond 
        ((and (hijos-hoja? l)
              (< 0 (length (hijos-arbol l)))) (list (cons (dato-arbol l) (length (hijos-arbol l)))))
        (else (subhijos-hoja-bosque (hijos-arbol l))
        )))

(check-equal? (subhijos-hoja iiii) '((c . 1) (g . 2) (h . 3)))


