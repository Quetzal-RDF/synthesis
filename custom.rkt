#lang rosette

(require rosette/lib/angelic)

(require "interp-enumerate.rkt")
(require "parse.rkt")
(require "expression-lexer.rkt")

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
                 (list 'send 'p 'logic-op (list 'append (list 'quote nested-pos) 'pos) (cadr l) (cadr r))
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

(define (make-custom-functions columns text do-all-int do-all-bool do-all-str do-all-any)
  (let* ((make-custom 
          (make-custom-function do-all-int do-all-bool do-all-str do-all-any))
         (parse
          (apply make-parser columns))
         (stuff
          (parse text)))
    (println make-custom)
    (map make-custom (filter cons? stuff))))

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

(define (test-custom text columns)
  (let* ((trace (new tracing-expr-processor% [inputs columns]))
         (p (new compound-processor%
                 [children
                  (list
                   (new doc-processor%)
                   trace)]))
         (pair (lambda (x) (list x x)))
         (int (lambda (size pos p f) (f 5 (pair (val pos integer?)))))
         (bool (lambda (size pos p f) (f 5 (pair (val pos boolean?)))))
         (str (lambda (size pos p f) (f 5 (pair (val pos string?))))))
    (values
     (map (lambda (f)
            (let ((expr '())
                  (doc '()))
              (f 5 '() p (lambda (x y) (println y) (set! expr (cadr y)) (set! doc (car y))))
              (cons doc expr)))
          (map car
               (make-custom-functions
                (map ~a columns)
                text
                int bool str
                (lambda (size pos p f)
                  ([choose* int bool str] size pos p f)))))
     (send trace controls))))

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
  (let* ((custom (make-custom-table text (map ~a symbolic)))
         (start (length (remove-duplicates (apply append (hash-values custom))))))
    (test-int analyze '() custom '() '() start (*  2 start) outputs symbolic inputs)))

(define (parse-generate-data tokens cols)
   (let-values ([(fs controls)
                (test-custom
                 tokens
                 cols)])
    (let ((result (for/list ([f fs])
      (list (car f) (generate-models (cdr f) controls #t)))))
      (create-table result cols))))

(define (generate-models expr controls extra)
  (letrec ((models (lambda (guards ctrls)
                     (if (null? ctrls)                         
                         (let ((result (solve (assert (and guards extra)))))
                           (if (sat? result)
                               (let* ((answer (evaluate expr result))
                                      (row1
                                       (if (term? answer)
                                           (let ((a1 (if (string? answer) "" 0)))
                                             (list a1 (hash->list (model (solve (assert (and guards extra (equal? expr a1))))))))
                                           (list answer (hash->list (model result)))))
                                      (result2
                                       (solve (assert (and guards extra (not (equal? expr (car row1))))))))
                                  (if (sat? result2)
                                     (list row1 (list (evaluate expr result2) (hash->list (model result2))))
                                     (list row1)))
                               '()))
                         (append
                          (models (and (car ctrls) guards) (cdr ctrls))
                          (models (and (not (car ctrls)) guards) (cdr ctrls)))))))
    (models #t controls)))

(define (create-table result cols)
    ; for each subexpression we have a list of models which correspond to rows of the table.  The first element in that list
    ; is the expected output for that subexpression, and the second element is a list of column bindings
  (let ((used-cols (gather-cols (flatten result) cols)))
    (cons used-cols
          (for/fold ([r '()]) ([e result])
            (append r
                    (for/fold ([rr '()]) ([m (cdr e)])
                      (println m)
                      (append rr (for/list ([row m])
                                   (let ((vec (make-vector (+ (length used-cols) 2))))
                                     (println row)
                                     (for ([cell (cadr row)])
                                       (let* ((col (car cell))
                                              (val (cdr cell))
                                              (pos (index-of used-cols col)))
                                         (vector-set! vec pos val)))
                                     (vector-set! vec (length used-cols) (car row))
                                     (vector-set! vec (+ 1 (length used-cols)) (car e))
                                     (vector->list vec))))))))))

(define (gather-cols result cols)
  (cond [(null? cols) '()]
        [(index-of result (car cols)) (cons (car cols) (gather-cols result (cdr cols)))]
        [#t (gather-cols result (cdr cols))]))
    

(define (parse-column-metadata p)
 (for/list ([i p])
      (let ((type (car (list-ref i 3)))
            (colName (cadr i)))
        (val (string->symbol colName)
             (cond [(= type 1) integer?]
                   ; type 2 is a date and needs to be changed because a symblic date type wont be accepted by Rosette
                   [(= type 2) integer?]
                   [(= type 3) string?]
                   [(= type 4) boolean?]
                   [(= type 5) real?]
                   [#t string?])))))


(provide test-custom make-custom-table analyze-custom generate-models parse-generate-data parse-column-metadata)