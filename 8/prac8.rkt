#lang racket
(require "lpp.rkt")
(require rackunit)

; 1
; a.1)
(define arbol-sin-barrera '(15 (4 (2)
                                  (3))
                               (8 (6))
                               (12 (9)
                                   (10)
                                   (11))))
;(pinta-arbol arbol-sin-barrera)

(define arbol
  (construye-arbol 15
                   (list (construye-arbol 4  (list (construye-arbol 2 '())
                                                   (construye-arbol 3 '())))
                         (construye-arbol 8  (list (construye-arbol 6 '())))
                         (construye-arbol 12 (list (construye-arbol 9 '())
                                                   (construye-arbol 10 '())
                                                   (construye-arbol 11 '()))))))
;(pinta-arbol arbol)
;(hoja-arbol? (construye-arbol 11 '()))

(check-equal? arbol arbol-sin-barrera)
(check-equal? (first (second (rest (third (rest arbol-sin-barrera))))) 10)
(check-equal? (first (second (hijos-arbol (third (hijos-arbol arbol))))) 10)









              