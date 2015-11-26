#lang racket
(require "fp2015/palindrome?.rkt")

;Implement a function, called (p-score n), which finds the palindrome score for n.
(define (p_score n)
  (define (helper i result)
    (cond [(palindrome? i) result]
          [(helper (+ i (reverse-int i)) (+ result 1))]))
  (helper n 1))
