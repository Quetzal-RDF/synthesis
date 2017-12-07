#lang racket

(define (call-racket-string-number tok)
  (string->number tok))

(provide call-racket-string-number)