#lang racket

(define (palindromo? cmp l)
  (if (or (null? l)
          (and (not (null? l)) (null? (rest l)) ))
      #t
      (and (cmp (first l) (ultimo-v2 l))
           (palindromo? cmp (quita-ultimo-v2 (rest l))) )))

(define (ultimo l)
  (first (reverse l)))

(define (ultimo-v2 l)
  (if (null? l)
      '()
      (if (null? (rest l))
          (first l)
          (ultimo (rest l)) )))

(define (quita-ultimo l)
  (reverse (rest (reverse l))))

(define (quita-ultimo-v2 l)
  (if (or (null? l)
          (null? (rest l)))
      '()
      (cons (first l)
            (quita-ultimo (rest l)))))

(palindromo? equal? '()) ; => #t
(palindromo? equal? '(a)) ; => #t
(palindromo? equal? '(a a)) ; => #t
(palindromo? equal? '(a t a)) ; => #t
(palindromo? equal? '(a t b)) ; => #f
(palindromo? equal? '(a t t b)) ; => #f
(palindromo? equal? '(a v e n i d a)) ; => #f
(palindromo? equal? '(l u z   a z u l)) ; => #t
(palindromo? = (list 1 0 1 0 1)) ; => #t
(palindromo? = (list 1 0 1 0 1 999999999)) ; => #f
