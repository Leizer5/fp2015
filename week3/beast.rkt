#lang racket
(require "week2/fence.rkt")

;Implement a function, called (nth-beast-number n), which returns the nth number of the beast.
(define (nth-beast-number n)
 (string->number (string-repeat "666" n)))
