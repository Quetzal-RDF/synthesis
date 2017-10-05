#lang rosette

(require rosette/lib/angelic)

(require "interp-enumerate.rkt")
(require "parse.rkt")
(require "expression-lexer.rkt")
(require "expression-writer.rkt")
(require rosette/solver/smt/z3)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (to-custom-int form nested-pos)
  (println form)
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
        [(extract-epoch-from-date)
         (let ((date (to-custom-int (cadr form) (cons 1 nested-pos))))
           (list (car date)
                 (list 'send 'p 'date-to-epoch (cadr date))
                 'number))
        ]
        [(date-subtract)
         (let ((date1 (to-custom-int (cadr form) (cons 1 nested-pos)))
               (date2 (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car date1) (car date2))
                 (list 'send 'p 'date-diff (cadr date1) (cadr date2))
                 'number))
        ]
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
         (let ((op (car form))
               (l (to-custom-int (second form) (cons 1 nested-pos)))
               (r (to-custom-int (third form) (cons 2 nested-pos))))
           (list (append (car l) (car r))
                 (list 'send 'p 'basic-binary op (cadr l) (cadr r))
                 'boolean))])
     (list '()
           (list 'send 'p 'constant form)
           'any)))

(define (to-custom form)
  (let ((x (to-custom-int form '())))
    (println x)
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
    (println make-custom)
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

(define (test-custom fragments columns)
  (let* ((pair (lambda (x) (list x x)))
         (int (lambda (size pos p f) (f 5 (pair (val pos integer?)))))
         (bool (lambda (size pos p f) (f 5 (pair (val pos boolean?)))))
         (str (lambda (size pos p f) (f 5 (pair (val pos string?))))))
    (map (lambda (f)
           (let* ((trace (new tracing-expr-processor% [inputs columns]))
                  (p (new compound-processor%
                          [children
                           (list
                            (new doc-processor%)
                            trace)]))
                  (expr '())
                  (doc '()))
             (f 5 '() p (lambda (x y) (println y) (set! expr (cadr y)) (set! doc (car y))))
             (list doc expr (send trace controls))))
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

(define (generate-data text cols columnMetadata)
  (let* ((parse (apply make-parser (map ~a cols)))
         (stuff (parse text))
         (fs (test-custom stuff cols))
         (result
          (for/list ([f (filter (lambda (v) (not (eq? (cadr v) 'invalid))) fs)])
            (list (car f) (generate-models (cadr f) (caddr f) #t)))))
    (create-table result cols columnMetadata)))

(define (generate-models expr controls extra)
  (println controls)
  (letrec ((v (constant (string->symbol (string-append "answer_" (~v (type-of expr)))) (type-of expr)))
            (solve (lambda (formula)
                    (let ((solver (z3)))
                      (solver-clear solver)
                      (solver-assert solver (list (and (equal? v expr) formula)))
                      (let ((x (solver-check solver)))
                        (solver-shutdown solver)
                        (println "formula")
                        (println (and (equal? v expr) formula))
                        x))))
           (models (lambda (guards ctrls)
                     (let ((result (solve (and guards extra))))
                       (if (sat? result)
                           (if (null? ctrls)                         
                               (let* ((answer (evaluate v result))
                                      (row1 (list answer (hash->list (model result))))
                                      (result2
                                       (solve (and guards extra (not (equal? expr (car row1)))))))
                                 (println row1)
                                 (println result2)
                                 (if (sat? result2)
                                     (list row1 (list (evaluate expr result2) (hash->list (model result2))))
                                     (list row1)))
                               (append
                                (models (and (car ctrls) guards) (cdr ctrls))
                                (models (and (not (car ctrls)) guards) (cdr ctrls))))
                           '())))))
    (models #t controls)))

(define (create-table result cols columnMetadata)
    ; for each subexpression we have a list of models which correspond to rows of the table.  The first element in that list
    ; is the expected output for that subexpression, and the second element is a list of column bindings
  (let ((used-cols (gather-cols (flatten result) cols)))
    (cons (map ~v used-cols)
          (filter (lambda (l) (not (null? l)))
           (for/fold ([r '()]) ([e result])
            (append r
                    (for/fold ([rr '()]) ([m (cdr e)])
                      (append rr (for/list ([row m])
                                   (let/ec return
                                     (let ((vec (make-vector (+ (length used-cols) 2))))
                                       (for ([cell (cadr row)])
                                         (let* ((col (car cell))
                                                (val (cdr cell))
                                                (pos (index-of used-cols col)))
                                           (if pos
                                               (vector-set! vec pos val)
                                               (return (list)))))
                                       (vector-set! vec (length used-cols) (car row))
                                       (vector-set! vec (+ 1 (length used-cols)) (to-html (car e) columnMetadata))
                                       (vector->list vec))))))))))))

(define (gather-cols result cols)
  (cond [(null? cols) '()]
        [(index-of result (car cols)) (cons (car cols) (gather-cols result (cdr cols)))]
        [#t (gather-cols result (cdr cols))]))

    

(define (parse-column-metadata p)
 (for/list ([i p])
      (let ((type (car (list-ref i 3)))
            (colName (cadr i)))
        (println type)
        (val (string->symbol (string-append colName "_" (~v type)))
             (cond [(= type 1) integer?]
                   ; type 2 is a date and needs to be changed because a symblic date type wont be accepted by Rosette
                   [(= type 2) integer?]
                   [(= type 3) string?]
                   [(= type 4) boolean?]
                   [(= type 5) real?]
                   [#t string?])))))

(define-symbolic s1 string?)

(define-symbolic i1 integer?)
(define-symbolic i2 integer?)
(define-symbolic i3 integer?)
(define-symbolic i4 integer?)
(define-symbolic i5 integer?)



(provide test-custom make-custom-table analyze-custom generate-models generate-data parse-column-metadata)