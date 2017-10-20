#lang rosette

(define/match (ite? e)
    [((expression op child ...)) (or (string=? "ite" (~v op)) (string=? "ite*" (~v op)))]
    [(_) #f])

(define-syntax for/all/*
  (syntax-rules ()
    ((_ ([val expr]) body ...)
     (for/all ([val expr])
       (letrec ((push
                 (lambda (v)
                   (if (or (union? v) (ite? v))
                       (for/all ([e v])
                         (push e))
                       (begin
                         body
                         ...)))))
         (push val))))))
