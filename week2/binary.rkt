#lang racket

;Implement the generic string reverse function.
(define (string-reverse str)
  (define (string-iter i result)
    (if (< i 0) result
        (string-iter (- i 1) (string-append result (~a(string-ref str i))))))
  (string-iter (- (string-length str) 1) ""))

;Implement a function, that takes an integer n and returns the binary representation of n as a string.
(define (to-binary-string n)
  (define (binary-iter i result)
    (cond
      [(= i 0) result]
      [(= (remainder i 2) 0) (binary-iter (quotient i 2) (string-append result (~a "0")))]
      [(= (remainder i 2) 1) (binary-iter (quotient i 2) (string-append result (~a "1")))]))
  (string-reverse (binary-iter n "" )))

;Implement a function, that takes a binary string representation and returns the integer number from it.
(define (from-binary-string binary-str)
  (define(helper result n base)
    (if (= n 0) result
        (helper (+ result (*(remainder n 10) base)) (quotient n 10) (* base 2))))
  (helper 0 (string->number binary-str) 1))
