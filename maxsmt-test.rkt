#lang rosette

(require "maxsmt.rkt")

(current-bitwidth #f)

(define (row)
  (define-symbolic* f1 real?)
  (define-symbolic* f2 real?)
  (list f1 f2))

(define (formulae row)
  (list (> (car row) 5.2) (> (cadr row) (car row))))

(define (formula row)
  (letrec ((f
            (lambda (l)
              (if (null? l) #t (and (car l) (f (cdr l)))))))
    (f (formulae row))))

(define r1 (row))
(define r2 (row))
(define r3 (row))
(define r4 (row))

(define rows (list r1 r2 r3 r4))

(define s
  (let ((solver (z3)))
    (solver-clear solver)
    (solver-minimize solver (minimize-row-differences rows))
    (solver-assert solver
       (list (make-row-conditions-different (map formulae rows))
             (make-row-conditions-different (map list (map formula rows))))) 
    (let ((x (solver-check solver)))
      (solver-shutdown solver)
      x)))

