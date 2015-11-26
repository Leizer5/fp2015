#lang racket

(define (last-digit n) (remainder n 10))

; Обръщане на цифрите на числото n

(define (reverse-int n)
  (define (rev-iter n result)
    (cond [(= n 0) result]
          [else (rev-iter (quotient n 10) (+ (* result 10) (last-digit n)))]))
  (rev-iter n 0))

; Най-простата проверка дали едно число е палиндром - дали е равно на своето "обърнато"

(define (palindrome? n)
  (= n (reverse-int n)))
