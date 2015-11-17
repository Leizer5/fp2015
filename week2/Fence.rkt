#lang racket
;This function should take a string and concatenate it with itself n times.

(define (string-repeat str n)
  (define (string-iter i result)
    (if (> i n) result
        (string-iter (+ i 1) (string-append result str))))
  (string-iter 1 ""))

;We are going to put number in fences. Fences represent a string that looks something like this:  {-->n<--}

(define (fence n)
  (string-append "{"
                 (string-repeat "-" (round (+ 1 (log n))))
                 ">"
                 (~a n)
                 "<"
                 (string-repeat "-" (round (+ 1 (log n))))
                 "}"))
