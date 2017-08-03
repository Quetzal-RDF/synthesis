#lang rosette

(require rosette/lib/angelic)

(require "interp-enumerate.rkt")
(require "parse.rkt")

(define (to-custom-int form)
         (println form)
  (if (list? form)
      (case (car form)
        [(in)
         (list '() (list 'send 'p 'in-v 'pos (second form) (lambda (x) #t)) 'any)]
        [(if)
         (case (length form)
           [(2)
            (let ((cond (to-custom-int (cadr form)))
                  (then (gensym 'p))
                  (else (gensym 'p)))
              (list (append (car cond) (list (list then 'any) (list else 'any)))
                    (list 'send 'p 'if-then-else (cadr cond) then else)
                    'any))]
           [(3)
            (let* ((cond (to-custom-int (second form)))
                   (then (to-custom-int (third form)))
                   (then-type (third then))
                   (else (gensym 'p)))
              (list (append (car cond) (car then) (list (list else then-type)))
                    (list 'send 'p 'if-then-else (cadr cond) (cadr then) else)
                    then-type))]
           [(4)
            (let* ((cond (to-custom-int (second form)))
                   (then (to-custom-int (third form)))
                   (then-type (third then))
                   (else (to-custom-int (fourth form)))
                   (type (if (eq? then-type 'any) (third else) then-type)))
              (list (append (car cond) (car then) (car else))
                    (list 'send 'p 'if-then-else (cadr cond) (cadr then) (cadr else))
                    type))])]
        [(not)
         (let ((op (car form))
               (l (to-custom-int (second form))))
           (list (car l) (list 'send 'p 'logic-op-not 'pos (cadr l)) 'boolean))]
        [(+ - * /)
         (let ((op (car form))
               (l (to-custom-int (second form)))
               (r (to-custom-int (third form))))
           (list (append (car l) (car r)) (list 'send 'p 'basic-math 'pos (cadr l) (cadr r)) 'number))]
        [(=)
         (let ((op (car form))
               (l (to-custom-int (second form)))
               (r (to-custom-int (third form))))
           (list (append (car l) (car r)) (list 'send 'p 'compare-to-str 'pos (cadr l) (cadr r)) 'boolean))]
        [(< >)
         (let ((op (car form))
               (l (to-custom-int (second form)))
               (r (to-custom-int (third form))))
           (list (append (car l) (car r)) (list 'send 'p 'compare-to 'pos (cadr l) (cadr r)) 'boolean))])
     (list '() form 'any)))

(define (to-custom form)
  (let ((x (to-custom-int form)))
    (println x)
    (values
     (map cadr (car x))
     (eval (quasiquote (lambda (unquote (append (list 'p 'pos) (map car (car x)))) (unquote (cadr x)))))
     (third x))))

(define (make-custom-function do-all-int do-all-bool do-all-str do-all-any)
  (lambda (fragment)
    (let-values ([(arg-types fun funtype) (to-custom fragment)])
      (apply custom fun
             (map
              (lambda (x)
                (case x
                  ((number) do-all-int)
                  ((boolean) do-all-bool)
                  ((string) do-all-str)
                  (else do-all-any)))
              arg-types)))))

(define (make-custom-functions columns text do-all-int do-all-bool do-all-str do-all-any)
  (let* ((make-custom 
          (make-custom-function do-all-int do-all-bool do-all-str do-all-any))
         (parse
          (apply make-parser columns))
         (stuff
          (parse text)))
    (println stuff)
    (map make-custom (filter cons? stuff))))

(define (test-custom text columns)
  (let ((p (new expr-processor% [inputs columns]))
        (int (lambda (size pos p f) (f 5 (val pos integer?))))
        (bool (lambda (size pos p f) (f 5 (val pos boolean?))))
        (str (lambda (size pos p f) (f 5 (val pos string?)))))
    (map (lambda (f)
           (let ((expr '()))
             (f 5 '() p (lambda (x y) (set! expr y)))
             expr))
         (make-custom-functions
          (map ~a columns)
          text
          int bool str
          (lambda (size pos p f)
            ([choose* int bool str] size pos p f))))))

(provide test-custom)