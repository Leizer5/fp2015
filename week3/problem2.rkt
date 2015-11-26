#lang racket
(require "fp2015/week2/binary.rkt" "fp2015/palindrome?.rkt")

;Implement a function, called (next-hack n) that takes an integer and returns the next hack number, that is bigger than n
(define (number-of-ones n)
  (define (helper i result)
    (cond [(= (quotient i 10) 0) (+ result 1)]
          [(= (remainder i 10) 1) (helper (quotient i 10) (+ result 1))]
          [(= (remainder i 10) 0) (helper (quotient i 10) result)]))
  (helper (string->number (to-binary-string n)) 0))

(define (next-hack n)
  (define (helper i)
    (if (and (> i n) (palindrome? (string->number (to-binary-string i))) (odd? (number-of-ones i)))
        i
        (helper (+ i 1))))
  (helper n))