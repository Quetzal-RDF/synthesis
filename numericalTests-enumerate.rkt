#lang rosette

(require "interp-enumerate.rkt")

; ceiling
(define (num-test8)
  (test analyze 'ceiling 5 '(26 5) '((25.3)(4.8))))

; quotient
(define (num-test9)
  (test analyze 'quotient 5 '(6 2) '((20 3)(17 8))))

; floor
(define (num-test10)
   (test analyze 'floor 5 '(-43 4) '((-42.8)(4.8))))

; sign
(define (num-test11)
   (test analyze 'sign 5 '(1 0 -1) '((42.8) (0) (-4.8))))

; truncate
(define (num-test12)
   (test analyze 'truncate 5 '(42 -4) '((42.8)(-4.8))))

; remainder
(define (num-test13)
  (test analyze 'remainder 5 '(2 3) '((20 3)(13 5))))

(define (num-testAll)
  (begin (num-test8) (num-test9) (num-test10) (num-test11) (num-test12) (num-test13))) 