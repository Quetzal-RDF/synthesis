#lang rosette

;(error-print-width 10000000000)

(current-bitwidth #f)

(define vals (make-hash))

(define (id pos)
  (format "~S" pos))

(define (val pos type)
  (if (hash-has-key? vals pos)
      (hash-ref vals pos)
      (let ((v (constant (id pos) type)))
        (hash-set! vals pos v)
        v)))

(define doc-processor%
  (class object%
    (super-new)
    
    (define/public (in v)
      (list 'in v))
    
    (define/public (symbolic pos type)
      (list 'sym pos type))
    
    (define/public (basic-math pos l r)
      (list '- l r))
    
    (define/public (index-of pos l r)
      (list 'index-of l r))
    
    (define/public (compare-to pos l r)
      (list '< l r))
    
    (define/public (strlength pos str)
      (list 'length str))
    
    (define/public (substr str l r)
      (list 'substring str l r))

    (define/public (concat pos left right)
      (list 'concat left right))

    (define/public (get-digits pos str)
      (list 'get-digits str))))

(define expr-processor%
  (class object%
    (init inputs)

    (super-new)
    
    (define input-vals inputs)
    
    (define/public (in v)
      (when (< (- v 1) (length input-vals))
        (list-ref input-vals (- v 1))))

    (define/public (symbolic pos type)
      (val pos type))

    (define/public (basic-math pos l r)
      (if (and (number? l) (number? r))
          (let ((m1 (val (cons 'm1 pos) boolean?))
                (m2 (val (cons 'm2 pos) boolean?)))
            ((if m1 (if m2 + -) (if (or (= r 0) m2) * /)) l r))
          'invalid))

    (define/public (index-of pos l r)
      (if (and (string? l) (string? r) (<= (string-length r) (string-length l)))
          (string-index-of l r)
          'invalid))

    (define/public (compare-to pos l r)
      (if (and (number? l) (number? r))
          (let ((islt (val (cons 'islt pos) boolean?))
                (iseq (val (cons 'iseq pos) boolean?)))
            (if iseq (if islt (<= l r) (>= l r)) (if islt (< l r) (> l r))))
          'invalid))

    (define/public (strlength pos str)
      (if (string? str)
          (string-length str)
          'invalid))

    (define/public (substr str l r)
      (if (and (string? str) (number? l) (number? r) (>= l 0) (<= l r) (<= r (string-length str)))
          (substring str l r)
          'invalid))

    (define/public (concat pos left right)
      (if (and (string? left) (string? right))
          (string-append left right)
          'invalid))

    (define/public (get-digits pos str)
      (let ((s1 (val (cons 'b str) string?))
            (s2 (val (cons 'd str) string?))
            (s3 (val (cons 'a str) string?)))
        (if (and (equal? str (string-append s1 s2 s3))
                 (regexp-match-exact? #rx"[A-Za-z]*" s1)
                 (regexp-match-exact? #rx"[A-Za-z]*" s3)
                 (regexp-match-exact? #rx"[0-9]+" s2))
            s2
            'invalid)))))

(define compound-processor%
  (class object%
    (init children)

    (super-new)
    
    (define processors children)

    (define/public (in v)
      (for/list ([p processors])
        (send p in v)))

    (define/public (symbolic pos type)
      (for/list ([p processors])
        (send p symbolic pos type)))
   
    (define/public (basic-math pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p basic-math pos l r)))
    
    (define/public (index-of pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p index-of pos l r)))
    
    (define/public (compare-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p compare-to pos l r)))
    
    (define/public (strlength pos strs)
      (for/list ([p processors] [str strs])
        (send p strlength pos str)))
    
    (define/public (substr strs left right)
      (for/list ([p processors] [str strs] [l left] [r right])
        (send p substr str l r)))

    (define/public (concat pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p concat pos l r)))

    (define/public (get-digits pos strs)
      (for/list ([p processors] [s strs])
        (send p get-digits pos s)))))


(define (do-all-str size pos p f)
  (do-in1 size pos p f)
  (do-in2 size pos p f)
  (do-in3 size pos p f)
  (do-concat size pos p f)
  (do-substring size pos p f)
  (do-get-digits size pos p f)
  ; (do-strv size pos p f)
  )

(define (do-all-int size pos p f)
  (do-in1 size pos p f)
  (do-in2 size pos p f)
  (do-in3 size pos p f)
  (do-intv size pos p f)
  (do-basic-math size pos p f)
  (do-index-of size pos p f)
  (do-length size pos p f))

(define (do-all-bool size pos p f)
  (do-compare-to size pos p f))

(define (do-in1 size pos p f) (f size (send p in 1)))

(define (do-in2 size pos p f) (f size (send p in 2)))

(define (do-in3 size pos p f) (f size (send p in 3)))

(define (do-intv size pos p f) (f size (send p symbolic (cons 'int pos) integer?)))

(define (do-strv size pos p f) (f size (send p symbolic (cons 'str pos) string?)))

(define (do-unary-op do-arg op size pos p f)
  (let ((sz (- size 1)))
    (when (> sz 0)
      (do-arg
       sz (cons 1 pos) p
       (lambda (new-size new-expr)
         (when (> new-size 0)
           (f new-size (dynamic-send p op pos new-expr))))))))

(define (do-binary-op do-arg1 do-arg2 op size pos p f)
  (let ((sz (- size 1)))
    (when (> sz 0)
      (do-arg1
       sz (cons 1 pos) p
       (lambda (left-size left-expr)
         (when (> left-size 0)
           (do-arg2
            left-size (cons 2 pos) p
            (lambda (right-size right-expr)
              (when (> right-size 0)
                (f right-size (dynamic-send p op pos left-expr right-expr)))))))))))

(define (do-ternary-op do-arg1 do-arg2 do-arg3 op size pos p f)
  (let ((sz (- size 1)))
    (when (> sz 0)
      (do-arg1
       sz (cons 1 pos) p
       (lambda (str-size str-expr)
         (when (> str-size 0)
           (do-arg2
            str-size (cons 2 pos) p
            (lambda (start-size start-expr)
              (when (> start-size 0)
                (do-arg3
                 start-size (cons 3 pos) p
                 (lambda (end-size end-expr)
                   (when (> end-size 0)
                     (f end-size (dynamic-send p op str-expr start-expr end-expr))))))))))))))

(define (do-get-digits size pos p f)
  (do-unary-op do-all-str 'get-digits size pos p f))

(define (do-concat size pos p f)
  (do-binary-op do-all-str do-all-str 'concat size pos p f))

(define (do-basic-math size pos p f)
  (do-binary-op do-all-int do-all-int 'basic-math size pos p f))

(define (do-index-of size pos p f)
  (do-binary-op do-all-str do-all-str 'index-of size pos p f))

(define (do-compare-to size pos p f)
  (do-binary-op do-all-int do-all-int 'compare-to size pos p f))

(define (do-length size pos p f)
  (do-unary-op do-all-str 'strlength size pos p f))

(define (do-substring size pos p f)
  (do-ternary-op do-all-str do-all-int do-all-int 'substr size pos p f))

(define (symbolic? x) 
  (or (union? x) (term? x)))

; limit - max size of expressions to search over in terms of primitive operations
; inputs -  a list of rows such as (6 3 3) (9 6 3) (where the first element of each row (e.g. 6 or 9) is the output and the inputs
; are two input columns per row
(define (analyze limit . inputs)
  ; goals - number of solutions wanted
  ; models - set of expressions returned by the search 
  (let ((solver (current-solver))
        (goal 1)
        (models (list))
        (i 0))
    ; exception handler code checks if we have a list, and if so returns the list, lets other exceptions bubble up, see raise at the bottom
    ; the function where the result is raised as a list
    (with-handlers ([(lambda (v) (pair? v)) (lambda (v) v)])
      ((let ((v (car (car inputs))))
         (cond ((boolean? v) do-all-bool)
               ((integer? v) do-all-int)
               (#t do-all-str)))
       ; do-all-bool/do-all-int etc will be called with limit, an empty list (list) to indicate the root node of the expression tree we are developing
       ; a list of processors which include 1 for printing (doc-processor) and 1 expression processor initialized with inputs per row (cdr inputs discards
       ; first element which is the output)
       ; and a lambda function is a callback function that gets invoked on y which is a list of expressions that the expression processor comes up with in the search
       ; Note the first expression in y is discarded in processing for formulas because it reflects the output of the doc processor
       limit (list)
       (new compound-processor%
            [children
             (cons
              (new doc-processor%)
              (for/list ([input inputs])
                (new expr-processor% [inputs (cdr input)])))])
       (lambda (x y)
         (solver-clear solver)
         (set! i (+ i 1))
         (let ((formula
                (for/fold ([formula #t])
                          ([out (map car inputs)]
                           [in (cdr y)])
                  ; collect up all expressions generated by the expression processor in y (cdr y)
                  ; and create a single condition in formula for a solver to assert on with ANDs linking across rows.
                   (and
                    formula
                    (cond ((eq? out #t) (eq? #t in))
                          ((eq? out #f) (eq? #f in))
                          ((number? out) (and (number? in) (= out in)))
                          (#t (equal? out in)))))))         
           ; (println formula)
           ; (println formula)
           ; solver assertions.  When we have a satisfiable model and we have reached the number of goal (or solutions) we want
           ; raise models, which will then be trapped by the handler code
            (solver-assert solver (list formula))
           (let* ((result (solver-check solver)))
             ; (when (and (sat? result) (evaluate formula result)) - the  (evaluate formula result) should not be necessary but Z3
             ; has bugs so check.
             (when (and (sat? result) (evaluate formula result))
               (set! models (cons (cons (car y) result) models))
               (set! goal (- goal 1))
               (when (= goal 0)
                 (raise models))))))))))

(define (aggregate limit results . inputs)
  (let ((solver (current-solver))
        (goal 1)
        (models (list))
        (i 0))
    (with-handlers ([(lambda (v) (pair? v)) (lambda (v) v)])
      ((cond ((boolean? (car results)) do-all-bool)
             ((number? (car results)) do-all-int)
             (#t do-all-str))
       limit (list)
       (new compound-processor%
            [children
             (cons
              (new doc-processor%)
              (for/list ([input inputs])
                (new expr-processor% [inputs input])))])
       (lambda (x y)
         (when (and (null? (filter void? y)) (null? (filter (lambda (x) (eq? x 'invalid)) y)))
           (solver-clear solver)
           (set! i (+ i 1))
           (let* ((op (cond ((boolean? (car results))
                             (let ((v (constant 'agg boolean?)))
                               (lambda (x y) (if v (and x y) (or x y)))))
                            ((number? (car results))
                             (let ((v1 (constant 'agg1 boolean?))
                                   (v2 (constant 'agg2 boolean?)))
                               (if v1 (if v2 + max) min))) 
                            (#t string-append)))
                  (formula
                   (for/fold ([formula null])
                             ([result results]
                              [in (cdr y)])
                      (if (null? formula)
                         (list in (equal? in result))
                         (let ((f (op (car formula) in)))
                           (list f (and (equal? f result) (cadr formula))))))))
             (solver-assert solver (cdr formula))
             (let* ((result (solver-check solver)))
               (when (and (sat? result) (evaluate formula result))
                 (set! models (cons (cons (car y) result) models))
                 (set! goal (- goal 1))
                 (when (= goal 0)
                   (raise models)))))))))))

(provide analyze aggregate)