#lang racket

; escape parens, vertical bars, semi-column , white space, commas
(define (make-col-name colName)
  (string->symbol (string-replace colName " " "_" #:all? #t)))

(define (make-col-name-for-date colName index)
  (string->symbol
   (string-append
    (string-replace colName " " "_" #:all? #t)
    (~v index))))

(provide make-col-name make-col-name-for-date)
