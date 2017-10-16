#lang rosette

(require "interp-enumerate.rkt")
(require "custom.rkt")

(define (benchmark)
  (let ((lines (file->lines "expressions3.txt" #:mode 'text)))
    (for/list ([line lines])
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs (test-custom (list exp) symbolics)))
      (get-rows fs))
 )))


(define (benchmark-synthesis)
    (let ((lines (file->lines "expressions4.txt" #:mode 'text)))
    (for/list ([line lines])
      (println "parsing")
      (println line)
      (println "*****")
      (let* ((exp-types (read (open-input-string line)))
             (exp (car exp-types))
             (columnMetadata (cadr exp-types))
             (cols (map cadr columnMetadata))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs (test-custom (list exp) symbolics))
             (rows (get-rows fs))
             (table (create-table rows cols columnMetadata))
             (inputs
              (for/list ([row table])
                (println table)
                (take row (- (length row) 2))))
             (outputs
              (for/list ([row table])
                (list-ref row (- (length row) 2)))))        
      (analyze-custom "" outputs symbolics inputs)))))
