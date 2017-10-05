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
        (map (lambda (f)
               (let ((doit
                      (lambda (f p)
                        (if (not (eq? (cadr f) 'invalid))
                            (list (car f) (generate-models (cadr f) (caddr f) p))
                            '()))))
                 (if (not (union? (cadr f)))
                     (doit f #t)
                     (apply append
                            (map
                             (lambda (x)
                               (let ((guard (car x))
                                     (expr (cdr x)))
                                 (doit (list (car f) expr (caddr f)) guard)))
                             (union-contents (cadr f)))))))
             fs)))))
             

