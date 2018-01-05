#lang rosette

(require rosette/lib/angelic)

(require "interp-enumerate.rkt")
(require "parse.rkt")
(require "expression-lexer.rkt")
(require "expression-writer.rkt")
(require "dates.rkt")
(require rosette/solver/smt/z3)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (to-custom-int form nested-pos)
  ; (println form)
  (if (list? form)
      (case (car form)
        [(in)
         (list '()
               (list 'send 'p 'in-v (list 'append (list 'quote nested-pos) 'pos) (second form) (lambda (x) #t))
               'any)]
        [(if)
         (case (length form)
           [(2)
            (let ((cond (to-custom-int (cadr form) (cons 1 nested-pos)))
                  (then (gensym 'p))
                  (else (gensym 'p)))
              ; car cond refers to the parameters for the cond expression if if x
              (list (append (car cond) (list (list then 'any) (list else 'any)))
                    (list 'send 'p 'if-then-else (cadr cond) then else)
                    'any))]
           [(3)
            (let* ((cond (to-custom-int (second form) (cons 1 nested-pos)))
                   (then (to-custom-int (third form) (cons 2 nested-pos)))
                   (then-type (third then))
                   (else (gensym 'p)))
              (list (append (car cond) (car then) (list (list else then-type)))
                    (list 'send 'p 'if-then-else (cadr cond) (cadr then) else)
                    then-type))]
           [(4)
            (let* ((cond (to-custom-int (second form) (cons 1 nested-pos)))
                   (then (to-custom-int (third form) (cons 2 nested-pos)))
                   (then-type (third then))
                   (else (to-custom-int (fourth form) (cons 3 nested-pos)))
                   (type (if (eq? then-type 'any) (third else) then-type)))
              (list (append (car cond) (car then) (car else))
                    (list 'send 'p 'if-then-else (cadr cond) (cadr then) (cadr else))
                    type))])]
        [(not)
         (let ((op (car form))
               (l (to-custom-int (second form))))
           (list (car l)
                 (list 'send 'p 'logic-op-not (list 'append (list 'quote nested-pos) 'pos) (cadr l))
                 'boolean))]
        [(+ - * /)
         (let ((op (car form))
               (l (to-custom-int (second form) (cons 1 nested-pos)))
               (r (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car l) (car r))
                 (list 'send 'p 'basic-binary op (cadr l) (cadr r))
                 'number))]
        [(sum average min max)
         (let ((op (car form))
               (e (to-custom-int (second form) (cons 1 nested-pos))))
           (list (car e)
                 (list 'send
                       'p
                       'aggregate-op
                       (list 'append (list 'quote nested-pos) 'pos)
                       (list 'quote 'integer)
                       (cadr e)
                       (case op
                         ((sum average) +)
                         ((min) min)
                         ((max) max))
                       (eq? op 'average))
                 (caddr e)))]
        [(index-of)
         (let ((op (car form))
               (l (to-custom-int (second form) (cons 1 nested-pos)))
               (r (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car l) (car r))
                 (list 'send 'p 'index-of (list 'append (list 'quote nested-pos)) (cadr l) (cadr r))
                 'number))]
        [(trim)
         (let ((op (car form))
               (v (to-custom-int (second form) (cons 1 nested-pos))))
           (list (car v)
                 (list 'send 'p 'trim  (list 'append (list 'quote nested-pos)) (cadr v))
                 'string))]
        [(strlength)
         (let ((op (car form))
               (v (to-custom-int (second form) (cons 1 nested-pos))))
           (list (car v)
                 (list 'send 'p 'basic-unary 'strlength (cadr v))
                 'number))]
        [(abs truncate sign ceiling floor lifted-round)
         (let ((op (car form))
               (v (to-custom-int (second form) (cons 1 nested-pos))))
           (list (car v)
                 (list 'send 'p 'basic-unary op (cadr v))
                 'number))]
        [(add-seconds add-minutes add-hours add-days add-months add-years subtract-seconds subtract-minutes subtract-hours
                      subtract-days subtract-months subtract-years)
         (case (length form)
           [(2)
            (let ((date (to-custom-int (cadr form) (cons 1 nested-pos)))
                  (interval (gensym 'p)))
              (list (append (car date) (list (list interval 'number)))
                    (list 'send 'p 'date-interval (cadr date) interval)
                    'date))]
           [(3)
            (let ((date (to-custom-int (cadr form) (cons 1 nested-pos)))
                  (interval (third form)))
              (list (append (car date) (car interval)))
                    (list 'send 'p 'date-interval (cadr date) (cadr interval))
                    'date)]
           )
        ]
        [(extract-seconds extract-minutes extract-hours extract-days extract-months extract-years extract-day-of-year extract-day-of-week)
         (let ((date (to-custom-int (cadr form) (cons 1 nested-pos))))
              (list (car date)
                    (list 'send 'p 'date-extract-bin (cadr date) (car form))
                    'number))
        ]
        [(extract-from-date)
         (let ((date (to-custom-int (cadr form) (cons 1 nested-pos))))
           (list (car date)
                 (list 'send 'p 'date-extract (cadr date))
                 'number))
        ]
        [(date-to-epoch)
         (let ((date (to-custom-int (cadr form) (cons 1 nested-pos))))
           (list (car date)
                 (list 'send 'p 'date-to-epoch (cadr date))
                 'number))
        ]
        [(date-from-epoch)
         (let ((epoch (to-custom-int (cadr form) (cons 1 nested-pos))))
           (list (car epoch)
                 (list 'send 'p 'date-from-epoch (cadr epoch))
                 'date))
        ]
        [(date-subtract)
         (let ((date1 (to-custom-int (cadr form) (cons 1 nested-pos)))
               (date2 (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car date1) (car date2))
                 (list 'send 'p 'date-diff (cadr date1) (cadr date2))
                 'number))
        ]
        [(is-null is-not-null)
         (let ((op (car form))
               (s1 (to-custom-int (cadr form) (cons 1 nested-pos))))
           (list (car s1) 
                 (list 'send 'p 'is-null-v? (eq? op 'is-null) (cadr s1) (list 'append (list 'quote nested-pos) 'pos))
                 'boolean))]
        [(concat)
         (let ((s1 (to-custom-int (cadr form) (cons 1 nested-pos)))
               (s2 (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car s1) (car s2))
                 (list 'send 'p 'concat (list 'append (list 'quote nested-pos) 'pos) (cadr s1) (cadr s2))
                 'string))]
        [(substr)
         (let ((s1 (to-custom-int (cadr form) (cons 1 nested-pos)))
               (i1 (to-custom-int (third form) (cons 2 nested-pos)))
               (i2 (to-custom-int (fourth form) (cons 3 nested-pos))))
           (list (append (car s1) (car i1) (car i2))
                 (list 'send 'p 'substr (cadr s1) (cadr i1) (cadr i2))
                 'string))]
        [(=)
         (let ((op (car form))
               (l (to-custom-int (second form) (cons 1 nested-pos)))
               (r (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car l) (car r))
                 (list 'send 'p 'basic-binary equal? (cadr l) (cadr r))
                 'boolean))]
        [(and or)
         (let ((op (car form))
               (l (to-custom-int (second form) (cons 1 nested-pos)))
               (r (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car l) (car r))
                 (list 'send 'p 'if-then-else (cadr l)
                       (if (eq? op 'and) (cadr r) (list 'send 'p 'constant #t))
                       (if (eq? op 'and) (list 'send 'p 'constant #f) (cadr r)))
                 'boolean))]
        [(< > <= >=)
         (let* ((op (car form))
                (l (to-custom-int (second form) (cons 1 nested-pos)))
                (r (to-custom-int (third form) (cons 2 nested-pos)))
                (type
                 (cond ((eq? (caddr l) 'date) 'date)
                       ((eq? (caddr r) 'date) 'date)
                       (#t 'number))))
               (list (append (car l) (car r))
                     (list 'send 'p 'basic-binary
                           (cond ((eq? type 'number) op)
                                 ((eq? op '<) date-lt)
                                 ((eq? op '<=) date-le)
                                 ((eq? op '>) date-gt)
                                 ((eq? op '>=) date-ge)
                                 (#t op))
                           (cadr l) (cadr r))
                     'boolean))])
               
     (list '()
           (list 'send 'p 'constant form)
           (cond ((number? form) 'number)
                 ((string? form) 'string)
                 ((vector? form) 'date)
                 ((boolean? form) 'boolean)
                 (#t 'any)))))

(define (to-custom form)
  (let ((x (to-custom-int form '())))
    ; (println x)
    (values
     (map cadr (car x))
     (eval (quasiquote (lambda (unquote (append (list 'p 'pos) (map car (car x)))) (unquote (cadr x)))) ns)
     (third x))))

(define (make-custom-function do-all-int do-all-bool do-all-str do-all-any)
  (lambda (fragment)
    (let-values ([(arg-types fun funtype) (to-custom fragment)])
      (cons (apply custom fun
                   (map
                    (lambda (x)
                      (case x
                        ((number) do-all-int)
                        ((boolean) do-all-bool)
                        ((string) do-all-str)
                        (else do-all-any)))
                    arg-types))
            funtype))))

(define (make-custom-functions columns fragments do-all-int do-all-bool do-all-str do-all-any)
  (let* ((make-custom 
          (make-custom-function do-all-int do-all-bool do-all-str do-all-any)))
    ; (println make-custom)
    (map make-custom (filter cons? fragments))))

(define tracing-expr-processor%
  (class expr-processor%
    (super-new)

    (define conditionals '())

    (define/public (controls)
      conditionals)

    (define (if-then-else case l r)
      (set! conditionals (cons case conditionals))
      (super if-then-else case l r))    
    (override if-then-else)))

(define tracing-aggregating-processor%
  (class aggregating-processor%
    (super-new)

    (inherit-field processors)

    (define/public (controls)
      (map (lambda (p) (send p controls)) processors))))

(define row-count 5)

(define (test-custom fragments columns)
  (let* ((pair (lambda (x) (list x (for/list ([i (range 0 (+ row-count 1))]) x))))
         (int (lambda (size pos p f) (f 5 (pair (val pos integer?)))))
         (bool (lambda (size pos p f) (f 5 (pair (val pos boolean?)))))
         (str (lambda (size pos p f) (f 5 (pair (val pos string?))))))
    (map (lambda (f)
           (let* ((cols
                   (for/list ([i (range 0 row-count)])
                     (for/list ([v columns])
                       (if (vector? v)
                           (apply
                            vector
                            (map (lambda (v)
                                   (define-symbolic* d (type-of v))
                                   d)
                                 (vector->list v)))
                           (begin
                             (define-symbolic* val (type-of v))
                             val)))))
                  (trace
                   (new tracing-aggregating-processor%
                        [children
                         (cons
                          (new tracing-expr-processor% [inputs columns])
                          (for/list ([cs cols])
                            (new tracing-expr-processor%
                                 [inputs cs])))]))
                  (p (new compound-processor%
                          [children
                           (list
                            (new doc-processor%)
                            trace)]))
                  (expr '())
                  (doc '()))
             (f 5 '() p (lambda (x y)
                          ; (println y)
                          (set! expr (cadr y)) (set! doc (car y))))
             (list doc expr (send trace controls) (cons columns cols))))
          (map car
               (make-custom-functions
                (map ~a columns)
                fragments
                int bool str
                (lambda (size pos p f)
                  ([choose* int bool str] size pos p f)))))))

(define (make-custom-table text column-names)
  (let ((customs
         (make-custom-functions
          column-names
          text
          do-all-int
          do-all-bool
          do-all-str
          do-all-any)))
    (make-hasheq
           (for/fold ([hs '()])
                     ([fs customs])
             (letrec ((add (lambda (e l)
                             (cond ((null? l) (list (cons (cdr e) (list (car e)))))
                                   ((eq? (caar l) (cdr e))
                                    (cons (cons (caar l)
                                                (cons (car e) (cdar l)))
                                          (cdr l)))
                                   (#t (cons (car l) (add e (cdr l))))))))
               (add fs hs))))))

(define (analyze-custom text outputs symbolic . inputs)
  (let* ((parse (apply make-parser (map ~a symbolic)))
         (stuff (parse text))
         (custom (make-custom-table stuff (map ~a symbolic)))
         (start (length (remove-duplicates (apply append (hash-values custom))))))
    (test-int analyze '() custom '() '() start (*  2 start) outputs symbolic inputs)))

(define (get-rows fs)
  (filter (lambda (l) (not (null? l)))
    (apply append
         (map (lambda (f)
                (let ((doit
                       (lambda (f p)
                         (if (not (eq? (cadr f) 'invalid))
                             (list (car f) (generate-models (cadr f) (caddr f) p))
                             '()))))
                  (if (not (union? (cadr f)))
                      (list (doit f #t))
                      (map
                       (lambda (x)
                         (let ((guard (car x))
                               (expr (cdr x)))
                           (doit (list (car f) expr (caddr f)) guard)))
                       (union-contents (cadr f))))))
              fs))))

(define (generate-data text cols columnMetadata)
  (let* ((parse (apply make-parser (map ~a cols)))
         (stuff (parse text))
         (used-cols (gather-cols stuff cols))
         (parse-2 (apply make-parser (map ~a used-cols)))
         (stuff-2 (parse-2 text)))
    
         (with-handlers ([exn:fail?
                           (lambda (e) (cons used-cols '()))])
           (let* ((fs (test-custom stuff-2 used-cols))
                  (actual-cols (gather-cols (map car fs) used-cols)))
             (cons actual-cols
                   (map (lambda (f) (to-table f #t)) fs))))))

(define (generate-models exprs controlss extra)
  (letrec ((solve (lambda (formula)
                    (let ((solver (z3)))
                      (solver-clear solver)
                      (solver-assert
                       solver
                       (cons
                        formula
                        (for/list ([expr exprs]
                                   [i (in-range 0 (length exprs))])
                          (if (solvable? (type-of expr))
                              (equal?
                               expr
                               (constant (string->symbol (string-append "answer_" (~v i) "_" (~v (type-of expr)))) (type-of expr)))
                              #t))))
                      (let ((x (solver-check solver)))
                        (solver-shutdown solver)
                        x))))
           (rotate (lambda (l) (append (cdr l) (list (car l)))))
           (models (lambda (guards ctrlss)
                     (let ((result (solve (and guards extra))))
                       (if (sat? result)
                           (if (or (null? ctrlss) (null? (car ctrlss)))                   
                               (let ((answers (map (lambda (expr) (evaluate expr result)) exprs)))
                                 (list answers result))
                               (let ((first-guard (caar ctrlss)))
                                 (or (if (not (null? (cdr ctrlss)))
                                         (let ((second-guard (not (caadr ctrlss))))
                                           (models (and first-guard second-guard guards) (map rotate (cddr ctrlss))))
                                         #f)
                                     (models (and first-guard guards) (map rotate (cdr ctrlss)))
                                     (models (and (not first-guard) guards) (map rotate (cdr ctrlss)))
                                     (models guards (map rotate (cdr ctrlss))))))    
                           #f)))))
    (let ((guard
           (for/fold ([guard #t])
                     ([e exprs])
             (and guard
                  (if (union? e)
                      (for/fold ([ee #f])
                                ([uc (union-contents e)])
                        (if (not (eq? 'invalid (cdr uc)))
                            (or (car uc) ee)
                            ee))
                      #t)))))
      (println "guard")
      (println guard)
      (models guard controlss))))

(define (non-default-answer answers)
  (println answers)
  (for/fold ([x #f])
            ([answer answers])
    (or x
        (cond ((string? answer) (not (equal? answer "")))
              ((number? answer) (not (= answer 0)))
              ((boolean? answer) answer)
              (#t #f)))))

(define (diversify rows)
  (and
   (if (not (null? (cdr rows)))
       (for/fold ([x #t])
                 ([c1 (car rows)]
                  [c2 (cadr rows)])
         (and x (not (equal? c1 c2))))
       #t)
   (if (not (null? (cddr rows)))
       (diversify (cdr rows))
       #t)))
  
(define (to-table f extra)
  (let* ((diff (diversify (cdar (cdr (cddr f)))))
         (answers (non-default-answer (cdr (cadr f))))
         (m (generate-models (cdr (cadr f)) (cdr (caddr f)) (and extra diff answers)))
         (models (if (and m (>= (length m) 1)) m (generate-models (cdr (cadr f)) (cdr (caddr f)) extra))))
    (assert (>= (length models) 1))
    (map (lambda (x y)
           (append x (list y) (list (evaluate (car f) (cadr models)))))
         (evaluate (cdr (cadr (cddr f))) (cadr models))
         (car models))))

(define (create-table result cols columnMetadata)
    ; for each subexpression we have a list of models which correspond to rows of the table.  The first element in that list
    ; is the expected output for that subexpression, and the second element is a list of column bindings
  (let ((used-cols (gather-cols result cols)))
    (println used-cols)
    (cons (map ~v used-cols)
          (to-table result cols))))

(define (gather-cols result cols)
  (println result)
  (letrec ((gather-int
            (lambda (result)
              (cond [(and (cons? result) (eq? (car result) 'in)) (list (- (cadr result) 1))]
                    [(list? result) (remove-duplicates (apply append (map (lambda (e) (gather-int e)) result)))]
                    [#t '()]))))
    (map (lambda (i) (list-ref cols i)) (gather-int result))))

(define (parse-column-metadata p)
  (let ((sym (lambda (colName type)
               (val (string->symbol colName)
                    (cond [(= type 1) integer?]
                          [(= type 3) string?]
                          [(= type 4) boolean?]
                          [(= type 5) real?]
                          [#t string?])))))
    (for/list ([i p])
      (let ((type (car (list-ref i 3)))
            (colName (cadr i)))
        (if (= type 2)
            (vector
             (sym (string-append colName "_s") 1)
             (sym (string-append colName "_m") 1)
             (sym (string-append colName "_h") 1)
             (sym (string-append colName "_dy") 1)
             (sym (string-append colName "_mn") 1)
             (sym (string-append colName "_yr") 1))
            (sym colName type))))))
      
(define-symbolic s1 string?)

(define-symbolic i1 integer?)
(define-symbolic i2 integer?)
(define-symbolic i3 integer?)
(define-symbolic i4 integer?)
(define-symbolic i5 integer?)



(provide to-table test-custom make-custom-table analyze-custom generate-models generate-data parse-column-metadata get-rows create-table)