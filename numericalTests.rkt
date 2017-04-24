#lang rosette

(require "interp-simple.rkt")

; max
(define (num-test1)
  (test  "max" '(30 30) '((30 4)(5 7))))

; min
(define (num-test2)
  (test  "min" '(5 5) '((30 4)(5 7))))

; average
(define (num-test3)
  (test "average" '(17 17) '((30 4)(4 7))))

; abs
(define (num-test4)
  (test "abs" '(25.5 5.3) '((-25.5)(5.3))))

; abs
(define (num-test5)
  (test "abs" '(25 5) '((-25)(5))))

; count
(define (num-test6)
  (test  "count" '(2 2) '((30 4)(5 7))))

; sum
(define (num-test7)
  (test  "sum" '(34 34) '((30 4)(4 7))))

; ceiling
(define (num-test8)
  (test  "ceiling" '(26 5) '((25.3)(4.8))))

; quotient
(define (num-test9)
  (test  "quotient" '(6 3) '((20 3)(10 3))))

; floor
(define (num-test10)
   (test  "floor" '(-43 4) '((-42.8)(4.8))))

; sign
(define (num-test11)
   (test  "sign" '(1 0 -1) '((42.8)(0)(-4.8))))

; truncate
(define (num-test12)
   (test  "truncate" '(42 4) '((42.8)(4.8))))

; remainder
(define (num-test13)
  (test  "remainder" '(2 1) '((20 3)(10 3))))

(define (num-testAll)
  (begin (num-test1) (num-test2) (num-test3) (num-test4) (num-test5) (num-test6) (num-test7) (num-test8) (num-test9) (num-test10) (num-test11) (num-test12) (num-test13))) 