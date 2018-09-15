#lang rosette

(define/match (ite? e)
    [((expression op child ...)) (or (string=? "ite" (~v op)) (string=? "ite*" (~v op)))]
    [(_) #f])

(define/match (expression? e)
    [((expression op child ...)) #t]
    [(_) #f])

(define-syntax for/all/*
  (syntax-rules ()
    ((_ ([val expr]) body ...)
     (letrec ((push
               (lambda (val)
                 (if (or (union? val) (ite? val))
                     (for/all ([e val])
                       (push e))
                     (begin
                       body
                       ...)))))
       (push expr)))))

(define/match (ite-cases e)
    [((expression op child ...))
     (if (or (string=? "ite" (~v op)) (string=? "ite*" (~v op)))
         (cdr child)
         '())]
    [(_) '()])

(define/match (ite-pred e)
    [((expression op child ...))
     (if (string=? "⊢" (~v op))
         (car child)
         '())]
    [(_) '()])

(define/match (ite-preds e)
    [((expression op child ...))
     (if (or (string=? "ite" (~v op)) (string=? "ite*" (~v op)))
         (map ite-pred child)
         '())]
    [(_) '()])

(define (get-cases x)
  (ite-preds (for/all ([z x #:exhaustive]) z)))

(define (constraints vars e)
  (remove '()
    (match e
      [(expression op child ...)
       (let ((my-vars (set-intersect child vars)))
         (if (and (= (length child) 2) (= (length (set-intersect child vars)) 1))
             (list (cons op child))
             (apply append (map (lambda (c) (constraints vars c)) child))))]
      [_ (list)])))

(provide ite? for/all/* get-cases)
