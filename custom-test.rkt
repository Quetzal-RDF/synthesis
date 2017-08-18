#lang rosette

(require "custom.rkt")

(define-symbolic s1 string?)

(define-symbolic i1 integer?)
(define-symbolic i2 integer?)
(define-symbolic i3 integer?)
(define-symbolic i4 integer?)
(define-symbolic i5 integer?)

(define (test1)
  (let-values ([(fs controls) (test-custom '("i2" "*" "i3") (list s1 i2 i3))])
    (let ((result (solve (assert (and (> i2 1) (< i2 15) (> i3 1) (< i3 15) (= 15 (car fs)))))))
      (println result)
      (assert (sat? result))
      (assert (= 15 (evaluate (* i2 i3) result))))))

(define (test2)
  (let-values ([(fs controls) (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3))])
    (let ((result (solve (assert (and (> i2 1) (< i2 17) (> i3 1) (< i3 17) (equal? s1 "bad") (= 17 (car fs)))))))
      (println result)
      (println controls)
      (assert (sat? result))
      (assert (= 17 (evaluate (+ i2 i3) result))))))
  
(define (test3)
  (let-values ([(fs controls) (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3))])
    (letrec ((test (lambda (guards ctrls)
                     (if (null? ctrls)                         
                         (let ((result (solve (assert (and guards (= (car fs) 17))))))
                           (assert (sat? result))
                           (list result))
                         (append
                          (test (and (car ctrls) guards) (cdr ctrls))
                          (test (and (not (car ctrls)) guards) (cdr ctrls)))))))
      (test #t controls))))
                          

(define (test4)
  (analyze-custom '("if" "s1" "==" "bad" "then" "i2" "*" "i3") 1 '(50 0) (list s1 i2 i3) '("bad" 5 10) '("good" 5 10)))

(define (test5)
  (analyze-custom '("if" "s1" "==" "bad" "," "i2" "*" "i3") 2 '(40 50 0) (list s1 i2 i3) '("bad" 4 10) '("bad" 5 10) '("good" 5 10)))

(define (test6)
   (analyze-custom '("s1" "==" "Committed" "and" "i1" ">=" 25 "," "i3" "-" "(" "i4" "*" "i5" ")")
                   3
                   '(5000 0 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 10 10000 100 50) '("Custom" 25 10000 100 50)))

(define (test7)
   (analyze-custom '("if" "s1" "==" "Committed" "and" "i1" ">=" 25 "," "i3" "-" "(" "i4" "*" "i5" ")")
                   2
                   '(5000 6000 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 25 10000 100 40) '("Custom" 25 10000 100 50)))

(define (test8)
   (analyze-custom '("if" "s1" "==" "Committed" "and" "i1" ">=" 25 "then" "i3" "-" "(" "i4" "*" "i5" ")")
                   1
                   '(5000 0 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 0 10000 100 50) '("Custom" 23 10000 100 50)))

(define (test10)
  (let-values ([(fs controls) (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3" "else" "if" "s1" "==" "good" "then" "i2") (list s1 i2 i3))])
    (letrec ((test (lambda (guards ctrls)
                     (if (null? ctrls)                         
                         (let ((result (solve (assert (and guards (= (car fs) 17))))))
                           (if (sat? result)
                               (list result)
                               '()))
                         (append
                          (test (and (car ctrls) guards) (cdr ctrls))
                          (test (and (not (car ctrls)) guards) (cdr ctrls)))))))
      (test #t controls))))
