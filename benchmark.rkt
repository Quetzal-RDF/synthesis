#lang rosette


(define (benchmark) 
  (let ((lines (file->lines "expressions.txt" #:mode 'text)))
    (for ([line lines])
          (let* ((exp-types (eval (read (open-input-string line))))
                 (exp (car exp-types))
                 (symbolics (parse-column-metadata (cadr exp-types))))
            

            ))))

