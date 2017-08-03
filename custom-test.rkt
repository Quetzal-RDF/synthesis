#lang rosette

(require "custom.rkt")

(define-symbolic s1 string?)

(define-symbolic i2 integer?)
(define-symbolic i3 integer?)

(define (test1)
  (let* ((fs (test-custom '("i2" "+" "i3") (list s1 i2 i3)))
         (result (solve (assert (and (= i2 5) (> i3 0) (= 17 (car fs)))))))
    (assert (sat? result))
    (assert (= 17 (evaluate (+ i2 i3) result)))
    (println result)))
