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

(define (write-to-file file s)
  (with-output-to-file file
    (lambda () (printf s) (newline)) #:exists 'append))

;; given the current benchmark5.txt file, if you run this you will sometimes see it generate (- (in 1) (sign (in 1)))
;; because all the generated data uses negative numbers.  It can then find a counterexample.  For other function that
;; get generated, they are correct (if overly complex) and so no counterexample exists
(define (benchmark-synthesis infile)
  (when (file-exists? (string-append infile ".out")) (delete-file (string-append infile ".out")))
  (let ((lines (file->lines infile #:mode 'text)))
    (for/list ([line lines])
      ; (println "parsing")
      (write-to-file (string-append infile ".out") (string-append "processing:" line))
    ;  (println "*****")
      (let* ((exp-types (read (open-input-string line)))
             (exp (walk-exp (car exp-types) modify-exp))
             (columnMetadata (cadr exp-types))
             (cols (map cadr columnMetadata))
             (symbolics (parse-column-metadata (cadr exp-types)))
             (fs  (with-handlers ([exn:fail?
                          (lambda (e) '())])
                    (test-custom (list exp) symbolics))))
        (println exp)
        (println (arg-in-expression? exp (lambda(x) (equal? x 'in))))
       ; (println "FINISHED CUSTOM CREATION")
       ; (println fs)
        (if (not (arg-in-expression? exp (lambda(x) (equal? x 'in))))
            #f
            (if (null? fs)
                (write-to-file (string-append infile ".out") (string-append "failed to create custom:" line))
                (for/list ([f fs])
                  (let* ((table (to-table f #t)))
                    (when table
                      (let* ((inputs
                              (for/list ([row table])
                                (take row (- (length row) 2))))
                             (outputs
                              (for/list ([row table])
                                (list-ref row (- (length row) 2))))
                             (custom (make-custom-table (list exp) cols)))
                        ; (println exp)
                        ; (println custom)
                        (let ((xsynthesized (apply analyze custom '() '() 5 outputs symbolics inputs)))
                          (for/all ([synthesized xsynthesized]) 
                            ; (println outputs)
                            (let ((result
                                   (for/fold ([v #f])
                                             ([s synthesized])
                                     (let ((x
                                            (if (not (null? (cadddr s)))
                                                (evaluate (caddr s) (cadddr s))
                                                (caddr s))))
                                       (write-to-file  (string-append infile ".out") (if (not (null? (cadddr s)))
                                                                                         (~v (evaluate (caddr s) (cadddr s))) "model is null"))
                                       (write-to-file (string-append infile ".out") (~v x))
                                       ;    (println (car (cadr f)))
                                       (or v
                                           (with-handlers ([exn:fail? (lambda (e) #f)])
                                             (unsat? (solve (assert (not (equal? x (car (cadr f)))))))))))))
                              (if result (write-to-file (string-append infile ".out") (string-append "solved: " (~v exp))) (write-to-file (string-append infile ".out") (string-append "failed synthesis: " (~v exp))))
                              result)))))))))))))

(define (arg-in-expression? exp f)
  ; (println exp)
  (cond [(equal? exp '()) #f]
        [(f exp) #t]
        [(pair? exp) (if (arg-in-expression? (car exp) f) #t
                         (arg-in-expression? (cdr exp) f))]
        [#t #f]))

(define (can-process-in-list? exp)
  ; (println exp)
  (cond [(equal? exp '()) #f]
        [(pair? exp) (if (and (equal? (car exp) 'in-list) (< (length (third exp)) 10)) #t
                         (can-process-in-list? (cdr exp)))]
        [#t #f]))

; converts an in-list into a set of nested ors
(define (convert-in-list base l)
  (if (null? (cdr l))
      (list '= base (car l))
      (list 'or (list '= base (car l)) (convert-in-list base (cdr l)))))

(define (modify-exp exp)
  (if (pair? exp)
      (if (can-process-in-list? exp)
          (convert-in-list (second exp) (third exp))
          exp)
      exp))

(define (walk-exp exp f)
  ; (println exp)
  (if (null? exp)
      '()
      (if (pair? exp)
          (cons (f (car exp)) (walk-exp (cdr exp) f))
          exp)))
          

(provide benchmark benchmark-synthesis)
