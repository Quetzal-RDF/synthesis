#lang racket

(define (call-racket-string-number tok)
  (string->number tok))

(define (trim-first-last-chars tok)
  (substring tok 1 (- (string-length tok) 1)))

(provide call-racket-string-number trim-first-last-chars)