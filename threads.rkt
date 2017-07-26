#lang rosette

(require racket/async-channel)

(define printer
  (thread
   (lambda ()
     (letrec ((loop
               (lambda ()
                 (println (thread-receive))
                 (loop))))
       (loop)))))

(define (engine stop-channel)
  (thread
   (lambda ()
     (letrec ((loop
               (lambda ()
                 (let* ((stuff (thread-receive))
                        (problem (car stuff))
                        (hook (cdr stuff)))
                   (unless (async-channel-try-get stop-channel)
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

(define (engines n)
  (let ((stop-channel (make-async-channel (+ n 1))))
    (cons
     stop-channel
     (for/list ([i (in-range n)])
       (engine stop-channel)))))

(define (engines-solve engines formula hook)
  (engine-solve (list-ref engines (+ (random (length (cdr engines))) 1)) formula hook))

(define (engines-stop engines)
  (for ([e (cdr engines)])
    (async-channel-put (car engines) #t)))

(provide engine engines engine-solve engines-solve engines-stop)
