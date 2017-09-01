#lang racket

(define (make-col-name colName)
  (string->symbol (string-replace colName " " "_" #:all? #t)))

(provide make-col-name)