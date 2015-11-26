#lang racket

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

; You should add two fractions together and return a new fraction
(define (add-frac frac1 frac2)
  (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))

; You should subtract two fractions together and return a new fraction
(define (substract-frac frac1 frac2)
  (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))

; You know the drill by now. Multiply and return a new fraction
(define (mult-frac frac1 frac2)
  (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))))

; Simplify, if possible, the given fraction
(define (simplify-frac frac)
  (cons (/ (fst frac) (gcd (fst frac) (snd frac))) (/ (snd frac) (gcd (fst frac) (snd frac)))))
