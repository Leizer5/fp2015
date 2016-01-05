#lang racket
;Implement the following Racket function, that groups consecutive equal elements into sub-lists:
(require "week5/problem-lists.rkt")

(define (group items)
  (cond [(null? items) '()]
        [else (cons
               (take-while (lambda (x) (equal? x (first items))) items)
               (group (drop-while (lambda (x) (equal? x (first items))) items)))]))
