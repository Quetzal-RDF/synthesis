#lang rosette

(require "interp-enumerate.rkt")
(require "custom.rkt")

(define (benchmark)
  (let ((lines (file->lines "expressions5.txt" #:mode 'text)))
    (for/list ([line lines])
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs (test-custom (list exp) symbolics)))
        (for/list ([f fs])
          (println (car (cadr f)))
          (to-table f #t))))))

;; given the current benchmark5.txt file, if you run this you will sometimes see it generate (- (in 1) (sign (in 1)))
;; because all the generated data uses negative numbers.  It can then find a counterexample.  For other function that
;; get generated, they are correct (if overly complex) and so no counterexample exists
(define (benchmark-synthesis)
    (let ((lines (file->lines "expressions5.txt" #:mode 'text)))
    (for/list ([line lines])
      (println "parsing")
      (println line)
      (println "*****")
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (columnMetadata (cadr exp-types))
             (cols (map cadr columnMetadata))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs (test-custom (list exp) symbolics)))
        (for/list ([f fs])
          (let* ((table (to-table f #t))
                 (inputs
                  (for/list ([row table])
                    (take row (- (length row) 2))))
                 (outputs
                  (for/list ([row table])
                    (list-ref row (- (length row) 2))))
                 (custom (make-custom-table exp cols)))
            (let ((synthesized (apply analyze custom '() '() 5 outputs symbolics inputs)))
              (for/list ([s synthesized])
                (let* ((check
                        (and
                         (letrec ((g (lambda (ss)
                                       (if (null? ss)
                                           #t
                                           (and
                                            (equal? (caar ss) (cdar ss))
                                            (g (cdr ss)))))))
                           (g (hash->list (model (fourth s)))))
                         (not (equal? (car (cadr f)) (third s)))))
                       (m (solve (assert check))))
                  (println check)
                  (if (sat? m)
                      (let ((a1 (evaluate (car (cadr f)) m))
                            (a2 (evaluate (third s) m)))
                        (list 'x (render s) a1 a2 (evaluate symbolics m)))
                      (render s)))))))))))
