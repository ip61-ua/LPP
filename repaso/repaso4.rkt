#lang racket
(require 2htdp/image)

;(foldl string-append "0" (list "1" "2" "3"))
(define (mi-foldl fn acc l)
  (if (null? l)
      acc
      (mi-foldl fn
                (fn (first l) acc)
                (rest l))))
;(mi-foldl string-append "0" (list "1" "2" "3"))

;(foldr string-append "0" (list "1" "2" "3"))

(define (mi-reverse l)
  (if (null? l)
      '() ; ojito que esto es una lista
      (append (mi-reverse (rest l))
              (list (first l)))))

(define (mi-foldr fn acc l)
  (foldl fn acc (mi-reverse l)))

;(mi-foldr string-append "0" (list "1" "2" "3"))

(define (sierpinski-base ancho)
  (above/align "center"
               (square ancho "outline" "olive")
               (beside (square ancho "outline" "olive")
                       (square ancho "outline" "olive"))))

(define (componer-sierpinski figura)
  (above/align "center"
               figura
               (beside figura figura)))

(define (sierpinski ancho)
  (if (< ancho 10)
      (sierpinski-base ancho)
      (componer-sierpinski (sierpinski (/ ancho 2)))))

; sierpinski => recursivo(ancho)
; sierpinski-base => base(ancho)
; componer-sierpinski => manejador de recursivo (recursivo(ancho))

;(sierpinski 400)

(define (hilbert-elem long-trazo)
  (beside/align "top"
                (line 0 long-trazo "black")
                (line long-trazo 0 "black")
                (line 0 long-trazo "black")))

(define (componer-hilbert figura long-trazo)
  (beside (above/align "left"
                       (beside/align "bottom"
                                     figura
                                     (line long-trazo 0 "black"))
                       (line 0 long-trazo "black")
                       (rotate -90 figura))
          (above/align "right"
                       figura
                       (line 0 long-trazo "black")
                       (rotate 90 figura))))

(define (hilbert nivel long-trazo)
  (if (= 1 nivel)
      (hilbert-elem long-trazo)
      (componer-hilbert (hilbert (- nivel 1) long-trazo) long-trazo)))

;(hilbert 3 30)

; hilbert => recursivo (*nivel, trazo)
; hilbert-elem => base (trazo)
; componer-hilbert => recursivo(recursivo-1, trazo)

(define (koch-elem trazo)
  (line trazo 0 "black"))

(define (componer-koch figura)
    (beside/align "bottom"
                figura
                (rotate 60 figura)
                (rotate -60 figura)
                figura))

(define (koch nivel trazo)
  (if (= 0 nivel)
      (koch-elem trazo)
      (componer-koch (koch (- nivel 1) trazo ))))

;(koch 4 10)

; koch => recursivo (*nivel0, trazo)
; koch-elem => base (trazo)
; componer-koch => recursivo (nivel-1, trazo)

(define (componer-copo-nieve figura)
  (above/align "center"
               figura
               (beside (rotate 120 figura)
                       (rotate -120 figura))))

(define (copo-nieve nivel trazo)
  (componer-copo-nieve (koch nivel trazo)))

;(copo-nieve 1 5)

(define (componer-alfombra figura circulo)
  (above (beside figura figura figura)
         (beside figura circulo figura)
         (beside figura figura figura)))

(define (alfombra-sierpinski tam)
  (if (<= tam 20)
      (circle (/ tam 2) "outline" "blue")
      (componer-alfombra (alfombra-sierpinski (/ tam 3))
                         (circle (/ tam 6) "solid" "blue"))))

(alfombra-sierpinski 600)




















