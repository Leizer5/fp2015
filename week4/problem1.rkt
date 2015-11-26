#lang racket

;Implement a function f that takes 3 arguments - a predicate p and two more functions - g and h. All function arguments are functions with one argument.
;f should return a function with a single argument.
;When the returned function is called, it should return #t if both p(g(x)) and p(h(x)) are true. #f should be returned otherwise.
;x is the argument of the returned from f function.
(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)))))
