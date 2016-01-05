#lang racket
(provide take-while drop-while)
; Намира сумата на всички числа в numbers
(define (sum numbers)
  (define (helper n result)
    (cond [(empty? n) result]
          [(helper (rest n) (+ (first n) result))]))
  (helper numbers 0))

 ; Проверява дали x се среща в items
(define (member? x items)
  (define (helper n)
    (cond [(empty? n) #f]
          [(equal? x (first n)) #t]
          [else (helper (rest n))]))
  (helper items))

; Връща дължина на items
(define (length2 items)
  (define (helper n result)
    (cond [(empty? n) result]
          [(helper (rest n) (+ result 1))]))
  (helper items 0))

; Връща n-тия елемент от items при 0лево базиран индекс
(define (list-ref2 items n)
  (define (helper x y)
    (cond [(= y 0) (first x)]
          [(helper (rest x) (- y 1))]))
    (helper items n))

; Връща всички цели числа в range (a b)
(define (range2 a b)
  (define (helper n result)
    (cond [(> n b) result]
          [(helper (+ n 1) (append result (list n)))]))
  (helper a '()))

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
(define (build-list2 n f)
  (define (helper i)
    (cond [(> i n) (list)]
          [else (cons (f i) (helper (+ 1 i)))]))
  (helper 0))

; Конкатенира два списъка в нов списък
(define (append23 l1 l2)
  (cond [(null? l1) l2]
        [else (cons (first l1) (append23 (rest l1) l2))]))

; Обръща списъка наобратно
(define (reverse2 items)
  (define (helper n result)
    (cond [(null? n) result]
          [else (helper (rest n) (cons (first n) result))]))
  (helper items '()))

; Взима първите n елемента от даден списък
(define (take2 n items)
  (cond [(> n (length items)) '()]
        [(= n 0) '()]
        [else (cons (first items) (take2 (- n 1) (rest items)))]))

; Маха първите n елемента от даден списък
(define (drop2 n items)
  (cond [(> n (length items)) '()]
        [(= n 0) items]
        [else (drop2 (- n 1) (rest items))]))

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
(define (take-while p items)
  (cond [(null? items) '()]
        [(not (p (first items))) '()]
        [else (cons (first items) (take-while p (rest items)))]))

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
(define (drop-while p items)
  (cond [(null? items) '()]
        [(not (p (first items))) items]
        [else (drop-while p (rest items))]))

; Функцията взима число и връща списък от цифрите му
(define (number->list n)
  (define (helper i result)
    (cond [(= i 0) result]
          [(helper (quotient i 10) (cons (remainder i 10) result))]))
  (helper n '()))

; Функцията взима списък от цифри и връща числото
(define (list->number ns)
  (define (helper i result)
    (cond [(empty? i) result]
          [(helper (rest i) (+ (first i) (* result 10)))]))
  (helper ns 0))
