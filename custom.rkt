#lang rosette

(require rosette/lib/angelic)

(require "interp-enumerate.rkt")
(require "parse.rkt")

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
     (eval (quasiquote (lambda (unquote (append (list 'p 'pos) (map car (car x)))) (unquote (cadr x)))))
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
                text
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
  (let* ((custom (make-custom-table text (map ~a symbolic)))
         (start (length (remove-duplicates (apply append (hash-values custom))))))
    (test-int analyze '() custom '() '() start (*  2 start) outputs symbolic inputs)))

(define (create-table models cols)
  (cons cols
    (for/list ([m models])
      (flatten (for/list ([column-bindings (cdr m)])
        (let ((vec (make-vector (length cols))))
          (for ([cell column-bindings])
            (let* ((col (car cell))
                   (val (cdr cell))
                   (pos (index-of cols col)))
              (when (int? pos)
                (vector-set! vec pos val))))
        (vector->list vec)))))))
      
      
      

(provide test-custom make-custom-table analyze-custom)