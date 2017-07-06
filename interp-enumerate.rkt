#lang rosette

;(error-print-width 10000000000)

(current-bitwidth #f)
(require math/base)
(require math/statistics)

(define vals (make-hash))

(define (sign b)
  (cond
    [(positive? b) 1]
    [(zero? b) 0]
    [(negative? b) -1]
))

(define (id pos)
  (string->symbol (format "~S" pos)))

(define (val pos type)
  (if (hash-has-key? vals pos)
      (hash-ref vals pos)
      (let ((v (constant (id pos) type)))
        (hash-set! vals pos v)
        v)))

(define logging-processor%
  (class object%
    (super-new)
    
    (define/public (in pos type-f)
      (if (eq? type-f number?)
          (list do-in-int)
          (list do-in-str)))
      	    
    (define/public (symbolic pos type)
      (list
       (if (equal? type integer?)
	   do-intv
	   do-strv)))
    
    (define/public (basic-math pos l r)
      (cons do-basic-math (append l r)))

    (define/public (basic-num-functions pos v)
      (cons do-basic-num-functions v))
    
    (define/public (index-of pos l r)
      (cons do-index-of (append l r)))
    
    (define/public (compare-to pos l r)
      (cons do-compare-to (append l r)))

    (define/public (compare-to-str pos l r)
      (cons do-compare-to-str (append l r)))
    
    (define/public (logic-op pos l r)
      (cons do-logic-op (append l r)))

    (define/public (logic-op-not pos v)
      (cons do-logic-op-not v))

    (define/public (if-then-else case l r)
      (cons (if (number? l) do-if-then-int do-if-then-str) (append case l r)))
    
    (define/public (strlength pos str)
      (cons do-length str))
    
    (define/public (substr str l r)
      (cons do-substring (append str l r)))

    (define/public (concat pos left right)
      (cons do-concat (append left right)))

    (define/public (get-digits pos str)
      (cons do-get-digits str))))

(define doc-processor%
  (class object%
    (super-new)
    
    (define/public (in pos type-f)
     (let ((m (val (cons 'argn pos) integer?)))
       (list 'in m)))
    
    (define/public (symbolic pos type)
      (val pos type))
    
    (define/public (basic-math pos l r)
      (let ((m1 (val (cons 'm1 pos) boolean?))
            (m2 (val (cons 'm2 pos) boolean?))
            (m3 (val (cons 'm3 pos) boolean?)))
        (list
         (cond [(and m1 m2 m3) 'quotient]
               [(and m1 m2 (not m3)) 'remainder]
               [(and m1 (not m2) m3) '/]
               [(and m1 (not m2) (not m3)) '*]
               [(and (not m1) m2 m3) '-]
               [#t '+])
         l r)))

    (define/public (basic-num-functions pos v)
      (let ((m1 (val (cons 'm1 pos) boolean?))
            (m2 (val (cons 'm2 pos) boolean?))
            (m3 (val (cons 'm3 pos) boolean?)))
            (list (if m1 (if m2 (if m3 'abs 'truncate) (if m3 'sign 'ceiling)) 'floor) v)))
    
    (define/public (index-of pos l r)
      (list 'index-of l r))
    
    (define/public (compare-to pos l r)
      (let ((islt (val (cons 'islt pos) boolean?))
            (iseq (val (cons 'iseq pos) boolean?)))
        (list (if iseq (if islt '<= '>=) (if islt '< '>)) l r)))

    (define/public (compare-to-str pos l r)
        (list '== l r))
    
    (define/public (logic-op pos l r)
      (let ((isand (val (cons 'isand pos) boolean?)))
        (list (if isand 'and 'or) l r)))

    (define/public (logic-op-not pos v)
        (list 'not v))

    (define/public (if-then-else case l r)
      (list 'if case l r))
    
    (define/public (strlength pos str)
      (list 'length str))
    
    (define/public (substr str l r)
      (list 'substring str l r))

    (define/public (concat pos left right)
      (list 'concat left right))

    (define/public (get-digits pos str)
      (list 'get-digits str))))

(define (basic-math-op r pos + - * / quotient remainder)
  (let ((m1 (val (cons 'm1 pos) boolean?))
        (m2 (val (cons 'm2 pos) boolean?))
        (m3 (val (cons 'm3 pos) boolean?)))
    (cond [(and m1 m2 m3 (integer? r) (not (= r 0))) quotient]
          [(and m1 m2 (not m3) (integer? r) (not (= r 0))) remainder]
          [(and m1 (not m2) m3 (not (= r 0))) /]
          [(and m1 (not m2) (not m3)) *]
          [(and (not m1) m2 m3) -]
          [#t +])))

(define expr-processor%
  (class object%
    (init inputs)

    (super-new)
    
    (define input-vals inputs)
    
    (define/public (in pos type-f)
      (let ((m (val (cons 'argn pos) integer?)))
        (if (and (>= m 1) (< (- m 1) (length input-vals)))
            (let ((v (list-ref input-vals (- m 1))))
              (if (type-f v)
                  v
                  'invalid))
            'invalid)))

    (define/public (symbolic pos type)
      (val pos type))

    (define/public (basic-math pos l r)
      (if (and (number? l) (number? r))
                ((basic-math-op r pos + - * / quotient remainder) l r)
          'invalid))

    (define/public (basic-num-functions pos v)
      (if (number? v)
          (let ((m1 (val (cons 'm1 pos) boolean?))
                (m2 (val (cons 'm2 pos) boolean?))
                (m3 (val (cons 'm3 pos) boolean?)))
            ((if m1 (if m2 (if m3 abs truncate) (if m3 sign ceiling)) floor) v))
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

    (define/public (compare-to-str pos l r)
      (if (and (string? l) (string? r))
          (equal? l r)
          'invalid))

    (define/public (logic-op-not pos v)
        (not v))
  
    (define/public (logic-op pos l r)
      (if (and (boolean? l) (boolean? r))
          (let ((isand (val (cons 'isand pos) boolean?)))
            (if isand (and l r) (or l r)))
           'invalid))
    
    (define/public (if-then-else case l r)
      (if (and (boolean? case))
          (if case l r)
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
    (init children ordering-function)

    (super-new)
    
    (define processors children)

    (define ordering ordering-function)

    (define/public (get-ordering-function)
      ordering)
    
    (define/public (in pos type-f)
      (for/list ([p processors])
        (send p in pos type-f)))

    (define/public (symbolic pos type)
      (for/list ([p processors])
        (send p symbolic pos type)))

    (define/public (basic-num-functions pos v)
      (for/list ([p processors] [vs v])
        (send p basic-num-functions pos vs)))

    (define/public (basic-math pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p basic-math pos l r)))
    
    (define/public (index-of pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p index-of pos l r)))
    
    (define/public (compare-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p compare-to pos l r)))

    (define/public (compare-to-str pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p compare-to-str pos l r)))

    (define/public (logic-op-not pos v)
      (for/list ([p processors] [vs v])
        (send p logic-op-not pos vs)))
  
    (define/public (if-then-else cases left right)
      (for/list ([p processors] [case cases] [l left] [r right])
        (send p if-then-else case l r)))
    
    (define/public (logic-op pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p logic-op pos l r)))
    
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

(define (do-all ops size pos p f)
  (for ([op ((send p get-ordering-function) ops)])
    (op size pos p f)))

(define (do-all-str size pos p f)
  (do-all
   (list do-in-str do-strv do-if-then-str do-concat do-substring do-get-digits)
   size pos p f))

(define (do-all-int size pos p f)
  (do-all
   (list do-in-int do-intv do-if-then-int do-basic-math do-index-of do-length do-basic-num-functions)
   size pos p f))

(define (do-all-bool size pos p f)
  (do-all
   (list do-compare-to do-compare-to-str do-logic-op do-logic-op-not)
   size pos p f))

(define (do-in-int size pos p f) (f size (send p in pos number?)))

(define (do-in-str size pos p f) (f size (send p in pos string?)))

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

(define (do-basic-num-functions size pos p f)
  (do-unary-op do-all-int 'basic-num-functions size pos p f))

(define (do-index-of size pos p f)
  (do-binary-op do-all-str do-all-str 'index-of size pos p f))

(define (do-compare-to size pos p f)
  (do-binary-op do-all-int do-all-int 'compare-to size pos p f))

(define (do-compare-to-str size pos p f)
  (do-binary-op do-all-str do-all-str 'compare-to-str size pos p f))

(define (do-logic-op size pos p f)
  (do-binary-op do-all-bool do-all-bool 'logic-op size pos p f))

(define (do-logic-op-not size pos p f)
  (do-unary-op do-all-bool 'logic-op-not size pos p f))

(define (do-if-then-str size pos p f)
  (do-ternary-op do-all-bool do-all-str do-all-str 'if-then-else size pos p f))

(define (do-if-then-int size pos p f)
  (do-ternary-op do-all-bool do-all-int do-all-int 'if-then-else size pos p f))

(define (do-length size pos p f)
  (do-unary-op do-all-str 'strlength size pos p f))

(define (do-substring size pos p f)
  (do-ternary-op do-all-str do-all-int do-all-int 'substr size pos p f))

(define (symbolic? x) 
  (or (union? x) (term? x)))

(define (convert-to-rational v)
  (if (real? v)
      (let ((parts (string-split (~v v) ".")))
        (if (= (length parts) 1)
            v
            (/ (with-input-from-string (apply string-append parts) (lambda () (read)))
               (expt 10 (string-length (cadr parts))))))
      v))

; limit - max size of expressions to search over in terms of primitive operations
; outputs - a list of values per row.  Assumption is output can be only one column
; inputs -  a list of rows such as (6 3 3) (9 6 3)
; symbolic - a symbolic formula that defines the formula which defines our result, which will be ultimately
; be used to create rows that either fit the formula or dont
; white - white list of functions
; black - black list of functions

(define (analyze white black limit outputs symbolic . inputs)
  ; goals - number of solutions wanted
  ; models - set of expressions returned by the search 
  (let ((solver (current-solver))
        (goal 1)
        (models (list))
        (i 0))
    ; exception handler code checks if we have a list, and if so returns the list, lets other exceptions bubble up, see raise at the bottom
    ; the function where the result is raised as a list
    (with-handlers ([(lambda (v) (pair? v)) (lambda (v) v)])
      ((let ((v (car outputs)))
         (cond ((boolean? v) do-all-bool)
               ((number? v) do-all-int)
               (#t do-all-str)))
       ; do-all-bool/do-all-int etc will be called with limit, an empty list (list) to indicate the root node of the expression tree we are developing
       ; a list of processors which include 1 for printing (doc-processor) and 1 expression processor initialized with inputs per row (cdr inputs discards
       ; first element which is the output), so we now have 1 expression processor per row.
       ; and a lambda function is a callback function that gets invoked on y which is a list of expressions that the expression processor comes up with in the search
       ; Note the first expression in y is discarded in processing for formulas because it reflects the output of the doc processor
       limit (list)
       (new compound-processor%
            [ordering-function
             (lambda (ops)
               (append
                (filter (lambda (op) (member op white)) ops)
                (filter (lambda (op) (not (member op (append black white)))) ops)))]
            [children
             (cons
              (new doc-processor%)
              (cons
               (new logging-processor%)
               (cons
                (new expr-processor% [inputs symbolic])
                (for/list ([input inputs])
                  ; initialize the field inputs in the expr-processor object with current row inputs.  Note the inputs in the following
                  ; line does not refer to the parameter passed into this function
                  (new expr-processor% [inputs input])))))])
       (lambda (x y)
         (with-handlers ([exn:fail? (lambda (e) '())])
         (solver-clear solver)
         (set! i (+ i 1))
         (let ((formula
                (for/fold ([formula #t])
                          ([out outputs]
                           [in (cdddr y)])
                  ; collect up all expressions generated by the expression processor in y (cddr y)
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
             (when (and (sat? result) (eq? #t (evaluate formula result)))
               (println result)
               (set! models (cons (list (car y) (remove-duplicates (cadr y)) (caddr y) result null) models))
               (set! goal (- goal 1))
               (when (= goal 0)
                 (raise models))))))))
      (list))))

(define (aggregate white black limit results symbolic . inputs)
  (let ((solver (current-solver))
        (goal 2)
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
                   (strop (cond ((boolean? (car results))
                             (let ((v (constant 'agg boolean?)))
                               (lambda (x y) (if v ('and) ('or)))))
                            ((number? (car results))
                             (let ((v1 (constant 'agg1 boolean?))
                                   (v2 (constant 'agg2 boolean?)))
                               (if v1 (if v2 '+ 'max) 'min))) 
                            (#t 'string-append)))
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
                 (set! models (cons (list (car y) result strop) models))
                 (set! goal (- goal 1))
                 (when (= goal 0)
                   (raise models))))))))
      (list))))

(define (render solution)
  (let* ((tree (list-ref solution 0))
         (model (list-ref solution 3))
         (agg (list-ref solution 4)))
    (cons (if (not (list? agg)) (evaluate agg model) 'notagg)
    (letrec ((print-tree
              (lambda (node)
                (cond [(list? node) (map print-tree node)]
                      [(symbolic? node)     ; symbolic variables
                       (let ((result (evaluate node model)))
                         (if (union? result)
                             (map cdr (union-contents result))
                             result))]
                      [#t node]))))
      (list (print-tree tree) (list-ref solution 1))))))

(define (testListMembers l1 l2)
  (if (equal? l2 '())
      '()
     (begin
       (assert (list? (member (car l2) (flatten l1))))
       (testListMembers l1 (cdr l2)))))

(define (check-operation l op)
  (println l)
  (when (list? l)
    (testListMembers l op)))

; func is the function to use - analyze or aggregates
; limit determines the total num of expressions it can use
; op is the top level node for the expression tree (for now) - to check if we got the right operation
(define (test func op white black limit raw-outputs symbolic raw-inputs)
   ; convert all reals to rational numbers in case we have any
  (let ((inputs (map (lambda(x) (map convert-to-rational x)) raw-inputs))
        (outputs (map convert-to-rational raw-outputs)))
    (letrec ((try-depth
              (lambda(v)
                (let ((out (apply func white black v outputs symbolic inputs)))
                  (if (null? out) (when (< v limit) (try-depth (+ 1 v))) (map render out))))))
      (let ((o (try-depth 2)))
        (print o)
        (check-operation o op)))))
    

(provide analyze render aggregate test val do-strv do-intv do-basic-num-functions do-index-of do-basic-math do-substring do-get-digits do-length)
