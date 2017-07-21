#lang rosette

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
                 (let* ((stuff (thread-receive))
                        (problem (car stuff))
                        (hook (cdr stuff)))
                   (unless (eq? problem 'stop)
                     (let ((solver (z3)))
                       (solver-clear solver)
                       (solver-assert solver (list problem))
                       (with-handlers ([exn:fail? (lambda (e) (println e))])
                         (let ((result (solver-check solver)))
                           (when (and (sat? result) (evaluate problem result))                             
                             (hook result))))
                       (solver-shutdown solver)
                       (loop)))))))
       (loop)))))

(define (engine-solve engine formula hook)
  (thread-send engine (cons formula hook)))

(define (engine-stop engine)
  (thread-send engine '(stop))
  (thread-wait engine))

(define (engines n) 
  (for/list ([i (in-range n)])
    (engine)))

(define (engines-solve engines formula hook)
  (engine-solve (list-ref engines (random (length engines))) formula hook))

(define (engines-stop engines)
  (for ([engine engines])
    (engine-stop engine)))

(provide engine engines engine-solve engines-solve engine-stop engines-stop)
