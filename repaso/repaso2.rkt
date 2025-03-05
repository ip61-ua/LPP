#lang racket

(require "lpp.rkt")

;-------------
;  2011 - 12  
;-------------

;; Ejercicio 2
; la original
(define p (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                (cons (cons 2 '())
                      (cons 1
                            (cons (cons 2 '())
                                  2)))))
; la traducida de la original a lists
(define p1 (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                 (cons (list 2)
                       (cons 1
                             (cons (list 2) 2)))))
; la modificada
(define p2 (cons (cons 5
                      (cons 7
                            (cons 8 7)))
                (cons (cons 2 '())
                      (cons 1
                            (cons 2
                                  (cons 2 '())
                                  )))))
; la modificada traducida
(define p3 (list (cons 5
                      (cons 7
                            (cons 8 7)))
                 (list 2)
                 1
                 2
                 2))

;(caja-puntero p)
;(caja-puntero p1)
;(caja-puntero p2)
;(caja-puntero p3)

; b) No es una lista porque el puntero que apunta al primer bloque
;    no puede definirse como tal.
;
;    A modo de estrategia, podemos fijarnos en las cajas que no
;    tienen punteros (aristas) salientes:
;    [8, 7]: no puede ser el final de una lista porque no termina
;     en lista vacía ('())
;    primer [2, '()]: es el final de una lista, por lo que puede
;     describirse como (list 2) al menos. Sin embargo, su puntero
;     entrante sale del primer elemento del bloque superior. Tratándose
;     así de (list 2).
;    segundo [2, '()]: (list 2) por el mismo razonamiento.
;
;    Véase las expresiones originales que se obtienen con p y p1.
;    Véase expresiones para considerar como lista a p2 y p3.
;
;    Pese a esto, el esquema sí contiene listas pero p *no apunta
;    directamente* a una de estas listas. Por lo que no podemos
;    considerar que p sea una lista, sino una pareja.

;; Ejercicio 3

; Funciones del ejemplo
(define (cuadrado x) (* x x))
(define (doble x) (* 2 x))
(define (suma-3 x) (+ 3 x))

; Resolución FOS
(define (mi-aplica-funcs n)
  (lambda (f) (f n)))

(define (resultados-funcs lista-funcs n)
  (map (mi-aplica-funcs n) lista-funcs)) ;; IMPORTANTE: (map <fn> <list>)

; Resolución recursiva
(define (mi-aplica-funcs-v2 l n)
  ((first l) n))

(define (volver-resultados-funcs-v2 l n)
  (resultados-funcs-v2 (rest l) n))

(define (resultados-funcs-v2-recursivo l n)
  (cons (mi-aplica-funcs-v2 l n)
        (volver-resultados-funcs-v2 l n)))

(define (resultados-funcs-v2 lista-funcs n)
  (if (null? lista-funcs)
      '()
      (resultados-funcs-v2-recursivo lista-funcs n)))

; Test
;(resultados-funcs-v2 (list cuadrado doble suma-3) 6) ;=> (36 12 9)

;; Ejercicio 4

; a)
;
; (define f ______)
; ((f) 5) -> ?
;
; b)
;
; (define (g x)
;   ______ )
;
; (define (f g)
;   ______ )
;
; ((f g) 3) → ?

; Propuesta

; a)

(define f (lambda () +))
;((f) 5) ; -> 5

; b)

; 1. Vemos que g depende de x.
(define (g x)
  (+ 255 x))

; 2. f depende de g
(define (f1 g)
  (lambda (y) (g y)))

; 3. la función f depende de la función g
;((f1 g) 3) ; -> 6

; Dado que en el apartado 3 observamos que se ejecuta un procedimiento del
; estilo ((algo) param) y no del tipo (algo param), como es "lo habitual".
; Tal y como ocurre en (f1 g) que es un procedimiento.
;
; Esto quiere decir que f1 devuelve una función. Es interesante resaltar
; que esta función tiene como parámetro g. La función g acepta un parámetro
; x.
;
; Si en el apartado 3, el parámetro de la función es una función y no su
; llamada; esto nos da la pista de que f1 se encargará del procesamiento
; g.
;
; A su vez, la función lambda que genere f1 será llamada con un 3. Cosa la
; cual nos indica que la función generada por f1 deberá tener un parámetro.
;
; A partir de aquí, sabemos de f1 lo siguiente:
;    - Devuelve una procedimiento que acepta un parámetro.
;    - Existe una relación en f1 con con el procedure g.
; 
; Podemos plantear que dentro de f1 se llega a llamar a g. La llamada a g
; debe producirse con un parámetro.
;
; Podemos aprovechar que f1 retorna una función que puede llamarse con un
; parámetro y hacer que ese parámetro lo procese g. Coincidiendo en como
; se invoca la función f1 en cuanto a número de argumentos se refiere.
;
; La solución propuesta final propone que f1 sea un "wrapper" de la función
; g.


;; Ejercicio 5

; a)
(define (primeros-n-recursivo l n)
  (cons (first l)
        (primeros-n (rest l)
                    (- n 1))))

(define (primeros-n lista n)
  (if (or (null? lista)
          (= n 0))
      '()
      (primeros-n-recursivo lista n)))

;(primeros-n '(1 2 3 4 5 6) 2) ; => (1 2)

; propuesta usando FOS
(define (n-ultimos-fos lista n)
  (reverse (primeros-n (reverse lista) n)))

; propuesta puramente recursiva
(define (n-ultimos lista n)
  (cond
    ((or (null? lista) (= n 0)) '())
    ((<= (length lista) n) lista)
    ((> (length lista) n) (n-ultimos (rest lista) n))))

;(n-ultimos '(1 2 3 4 5 6) 3) ; => (4 5 6)

; b)

(define (no-puedo-seguir l s)
  (or (null? l) (= s 0)))

(define (mi-intercalar-recursivo l s)
  (mi-intercalar (rest l) (- s 1)))


(define (mi-intercalar l s)
  (if (no-puedo-seguir l s)
      '()
      (cons (first l)
            (cons (first (n-ultimos l s))
                  (mi-intercalar-recursivo l s) ))))

(define (intercalar lista)
  (mi-intercalar lista (/ (length lista) 2)))

;(intercalar '(1 2 3 4 a b c d)) ; => '(1 a 2 b 3 c 4 d)
;(intercalar '()) ; => '()
;(intercalar '(1 a)) ; => '(1 a)
;(intercalar '(1 2 a b)) ; => '(1 a 2 b)

;; Ejercicio 6

; a)

(define palos-cartas '(oros copas espadas bastos))

(define (construye-baraja-aux palo n)
  (if (>= n 13)
      '()
      (cons (cons n palo) (construye-baraja-aux palo (+ n 1)) )))

(define (construye-baraja)
  (foldl (lambda (cur acc) (append acc (construye-baraja-aux cur 1)))
         (construye-baraja-aux (first palos-cartas) 1)
         (rest palos-cartas)
         ))

;(construye-baraja)

; b)

(define suma-solo-filt
  (lambda (cur acc) (+ acc (car cur))))

(define (filter-palos filt l)
  (filter (lambda (x) (equal? (symbol->string filt)
                              (symbol->string (cdr x)))) l))

(define (total-palo filt l)
  (foldr suma-solo-filt
         0
         (filter-palos filt l)
         ))

(total-palo 'bastos '((3 . oros)(4 . bastos)(1 . espadas)(8 . bastos))) ; → 12