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

(provide ite? for/all/*)
