#lang racket

(define (cross f all)
  (apply append (map (lambda (l) (map (lambda (r) (f l r)) (cdr (memq l all)))) all)))

(define (get-constraints ll)
    (letrec ((get (lambda (l i)
                    (if (null? l)
                        '()
                        (cons (list-ref (car l) (modulo i (length (car l))))
                              (get (cdr l) (+ i 1)))))))
      (get ll 0)))

(provide cross get-constraints)
