#! /usr/bin/env racket
#lang rosette
(require "benchmark.rkt")
(define MAX-BYTES (* 1 1024 1024 1024))
(custodian-limit-memory (current-custodian) MAX-BYTES)
(benchmark-synthesis (vector-ref (current-command-line-arguments) 0))

