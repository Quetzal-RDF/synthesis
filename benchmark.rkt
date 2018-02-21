#lang rosette

(require "interp-enumerate.rkt")
(require "custom.rkt")

(define (benchmark)
  (let ((lines (file->lines "tests.txt" #:mode 'text)))
    (for/list ([line lines])
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs (test-custom (list exp) symbolics)))
        (for/list ([f fs])
          (println "f")
          (println f)
          (to-table f #t))))))

(define (write-to-file s)
  (with-output-to-file "benchmark.out"
    (lambda () (printf s) (newline)) #:exists 'append))

;; given the current benchmark5.txt file, if you run this you will sometimes see it generate (- (in 1) (sign (in 1)))
;; because all the generated data uses negative numbers.  It can then find a counterexample.  For other function that
;; get generated, they are correct (if overly complex) and so no counterexample exists
(define (benchmark-synthesis f)
  (when (file-exists? "benchmark.out") (delete-file "benchmark.out"))
  (let ((lines (file->lines f #:mode 'text)))
    (for/list ([line lines])
      ; (println "parsing")
      (write-to-file (string-append "processing:" line))
    ;  (println "*****")
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (columnMetadata (cadr exp-types))
             (cols (map cadr columnMetadata))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs  (with-handlers ([exn:fail?
                          (lambda (e) '())])
                    (test-custom (list exp) symbolics))))
       ; (println "FINISHED CUSTOM CREATION")
       ; (println fs)
        (if (null? fs)
            (write-to-file (string-append "failed to create custom:" line))
            (for/list ([f fs])
              (let* ((table (to-table f #t))
                     (inputs
                      (for/list ([row table])
                        (take row (- (length row) 2))))
                     (outputs
                      (for/list ([row table])
                        (list-ref row (- (length row) 2))))
                     (custom (make-custom-table (list exp) cols)))
               ; (println exp)
               ; (println custom)
                (let ((synthesized (apply analyze custom '() '() 5 outputs symbolics inputs)))
                  ; (println outputs)
                  (let ((result
                         (for/fold ([v #f])
                                   ([s synthesized])
                           (let ((x
                                  (if (not (null? (cadddr s)))
                                      (evaluate (caddr s) (cadddr s))
                                      (caddr s))))
                             (write-to-file  (if (not (null? (cadddr s)))
                                      (~v (evaluate (caddr s) (cadddr s))) "model is null"))
                             (write-to-file (~v x))
                         ;    (println (car (cadr f)))
                             (or v (unsat? (solve (assert (not (equal? x (car (cadr f))))))))))))
                    (if result (write-to-file (string-append "solved: " (~v exp))) (write-to-file (string-append "failed synthesis: " (~v exp))))
                    result)))))))))

(provide benchmark benchmark-synthesis)
