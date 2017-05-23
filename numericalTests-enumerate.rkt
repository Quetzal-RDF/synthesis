#lang rosette

(require "interp-enumerate.rkt")
; multiply
(define (num-test1)
  (test analyze '('*) 5 '(8 2) '((40 .2)(20 .1))))

; divide
(define (num-test2)
  (test analyze '('/) 5 '(4.04 2) '((40.4 10)(18 9))))

; divide
(define (num-test3)
  (test analyze '('+) 5 '(50.4 27) '((40.4 10)(18 9))))

; subtract
(define (num-test4)
  (test analyze '('-) 5 '(30.4 9) '((40.4 10)(18 9))))

; 3*Col1 + Col2
(define (num-test5)
  (test analyze '('* '+) 5 '(13 18) '((3 4)(4 6))))

; (Col1 + Col2) * Col3
(define (num-test6)
  (test analyze '('* '+) 5 '(60 20) '((10 20 2)(4 6 2))))

; (Col1 + Col2) * 5
(define (num-test7)
  (test analyze '('* '+) 5 '(150 50) '((10 20)(4 6))))

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
  (begin (num-test1) (num-test2) (num-test3) (num-test4) (num-test5) (num-test6) (num-test7) (num-test8) (num-test9) (num-test10) (num-test11) (num-test12) (num-test13) (agg-1)
       (agg-2) (agg-3) (agg-4) (agg-5))) 