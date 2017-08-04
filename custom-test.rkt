#lang rosette

(require "custom.rkt")

(define-symbolic s1 string?)

(define-symbolic i2 integer?)
(define-symbolic i3 integer?)

(define (test1)
  (let* ((fs (test-custom '("i2" "+" "i3") (list s1 i2 i3)))
         (result (solve (assert (and (> i2 1) (< i2 17) (> i3 1) (< i3 17) (= 17 (car fs)))))))
    (println result)
    (assert (sat? result))
    (assert (= 17 (evaluate (+ i2 i3) result)))))

(define (test2)
  (let* ((fs (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3)))
         (result (solve (assert (and (> i2 1) (< i2 17) (> i3 1) (< i3 17) (equal? s1 "bad") (= 17 (car fs)))))))
    (println result)
    (assert (sat? result))
    (assert (= 17 (evaluate (+ i2 i3) result)))))

(define (test3)
  (let* ((fs (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3)))
         (result (solve (assert (and (not (equal? s1 "bad")) (= 17 (car fs)))))))
    (println result)
    (assert (sat? result))))

(define (test4)
  (analyze-custom '("if" "s1" "==" "bad" "then" "i2" "*" "i3") 2 '(50 0) (list s1 i2 i3) '("bad" 5 10) '("good" 5 10)))

(define (test5)
  (analyze-custom '("if" "s1" "==" "bad" "," "i2" "*" "i3") 5 '(50 0) (list s1 i2 i3) '("bad" 5 10) '("good" 5 10)))
