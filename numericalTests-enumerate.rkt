#lang rosette

(require "interp-enumerate.rkt")

; ceiling
(define (num-test8)
  (test analyze '('ceiling) 5 '(26 5) '((25.3)(4.8))))

; quotient
(define (num-test9)
  (test analyze '('quotient) 5 '(6 2) '((20 3)(17 8))))

; floor
(define (num-test10)
   (test analyze '('floor) 5 '(-43 4) '((-42.8)(4.8))))

; sign
(define (num-test11)
   (test analyze '('sign) 5 '(1 0 -1) '((42.8) (0) (-4.8))))

; truncate
(define (num-test12)
   (test analyze '('truncate) 5 '(42 -4) '((42.8)(-4.8))))

; remainder
(define (num-test13)
  (test analyze '('remainder) 5 '(2 3) '((20 3)(13 5))))

; max
(define (agg-1)
  (test aggregate '('max) 5 '(1 2 3) '((1)(2)(3))))

; max (+ in1 1)
(define (agg-2)
  (test aggregate '('max '+) 5 '(2 3 4) '((1)(2)(3))))

; string-append concat
(define (agg-3)
  (test aggregate '('string-append 'concat) 5 '("aa" "aabb" "aabbcc") '(("a")("b")("c"))))

; string-append substring
(define (agg-4)
  (test aggregate '('string-append 'substring) 5 '("aa" "aabb" "aabbcc") '(("aax")("bbx")("ccx"))))

; these two do not work and I dont know why
(define (agg-5)
  (test aggregate '('+ '+ '/) 5 '(9 27) '((10 .1)(20 .1))))

; this next one does not work and I dont know why
;(println (aggregate 5 '(9.45 27.05) '(10 .1 .05) '(20 .2 .1)))

(define (num-testAll)
  (begin (num-test8) (num-test9) (num-test10) (num-test11) (num-test12) (num-test13) (agg-1)
       (agg-2) (agg-3) (agg-4))) 