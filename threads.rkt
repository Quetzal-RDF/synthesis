#lang rosette

(require rosette/solver/smt/z3)

(define printer
  (thread
   (lambda ()
     (letrec ((loop
               (lambda ()
                 (println (thread-receive))
                 (loop))))
       (loop)))))

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

(provide engine engines engine-solve engines-solve engine-stop engines-stop)
