#lang rosette

(require rosette/solver/smt/z3)
(require data/heap)

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

(define (engine)
  (thread
   (lambda ()
     (letrec ((loop
               (lambda ()
                 (with-handlers ([exn:break? (lambda (x) '())])
                   (let* ((stuff (thread-receive))
                          (problem (car stuff))
                          (hook (cdr stuff))
                          (solver (z3)))
                     (with-handlers ([exn:break? (lambda (x) (solver-shutdown solver))])
                       (solver-clear solver)
                       (solver-assert solver (list problem))
                       (with-handlers ([exn:fail?
                                        (lambda (e) (if (exn:break? e)
                                                        (raise e)
                                                        (begin
                                                          (println e)
                                                          (hook problem e))))])
                         (let ((result (solver-check solver)))
                           (hook problem result)))
                       (solver-shutdown solver)
                       (loop)))))))
       (loop)))))

(define (engine-solve engine formula hook)
  (thread-send engine (cons formula hook)))

(define (engine-stop engine)
  (break-thread engine))

(define (engines n)
  (for/list ([i (in-range n)])
    (engine)))

(define (engines-solve engines formula hook)
  (engine-solve (list-ref engines (random (length (cdr engines)))) formula hook))

(define (engines-stop engines)
  (for ([e engines])
    (engine-stop e)))

(define balance-size 10)

(define engines%
  (class object%
    (super-new)

    (init n)

    (define smt-solvers (engines n)) 

    (define problem-queue (problem-heap))

    (define (solve-one)
      (let ((x (heap-min problem-queue)))
        (heap-remove! problem-queue x)
        (engines-solve smt-solvers (send x get-problem) (send x get-callback))))

    (define/public (solve formula priority hook)
      (heap-add! problem-queue (new problem% [smt formula] [priority priority] [hook hook]))
      (when (> (heap-count problem-queue) balance-size)
        (solve-one)))

    (define/public (drain)
      (if (> (heap-count problem-queue) 0)
          (begin
            (solve-one)
            (drain))
          (engines-stop smt-solvers)))))

(provide engine engines engine-solve engines-solve engine-stop engines-stop engines%)
