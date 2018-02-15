#lang rosette

(require data/heap)

(define balance-size 10)

(define problem%
  (class object%
    (super-new)

    (init smt priority hook)

    (define smt-problem smt)
    (define problem-priority priority)
    (define solution-callback hook)

    (define/public (get-callback)
      solution-callback)
    
    (define/public (get-problem)
      smt-problem)
    
    (define/public (get-priority)
      problem-priority)))

(define (problem<? l r)
  (< (send l get-priority) (send r get-priority)))

(define (problem-heap)
  (make-heap problem<?))

(define local-engines%
  (class object%
    (super-new)

    (init n)

    (define problem-queue (problem-heap))

    (define (solve-one)
      (let ((x (heap-min problem-queue)))
        (heap-remove! problem-queue x)
        (let ((z3 (z3))
              (problem (send x get-problem)))
          (solver-clear z3)
          (solver-assert z3 (list problem))
          (let ((result (solver-check z3)))
            ((send x get-callback) problem result)))))

    (define/public (solve formula priority hook)
      (heap-add! problem-queue (new problem% [smt formula] [priority priority] [hook hook]))
      (when (> (heap-count problem-queue) balance-size)
        (solve-one)))

    (define/public (drain)
      (when (> (heap-count problem-queue) 0)
        (solve-one)
        (drain)))))

(provide local-engines%)
