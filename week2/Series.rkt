#lang racket
;This function should return the nth member of the series that follow the pattern:
;A1 = a
;A2 = b
;An = An-1 + An-2 (сумата на предишните 2 числа)

(define (series a b n)
  (define (series-iter a b i)
    (if (> i n) b
        (series-iter b (+ a b) (+ i 1))))
    (cond
      [(= n 1) a]
      [(= n 2) b]
      [else (series-iter a b 3)]))

;returns the nth member of the Lucas series.
(define (lucas n)
  (series 2 1 n))

;returns the nth member of the Fibonacci series.
(define (fibonacci n)
  (series 1 1 n))

;returns the sum of the nth member of Lucas and nth member of Fibonacci series.
(define (summed-member n)
  (+ (lucas n) (fibonacci n)))

;return the sum of all members from the first to the nth one.
(define (nth-lucas-sum n)
  (define (iter i result)
    (if (> i n) result
        (iter (+ i 1)(+ result (lucas i)))))
  (iter 1 0))
(define (nth-fibonacci-sum n)
  (define (iter i result)
    (if (> i n) result
        (iter (+ i 1)(+ result (fibonacci i)))))
  (iter 1 0))

;returns the difference between the nth member of the Lucas series adn the nth member of Fibonacci.
(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n)))
