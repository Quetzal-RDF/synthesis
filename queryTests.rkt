#lang rosette

(require "./interp-enumerate.rkt")

; test simple inner join - Join on column 1 in table 1 and 2, project column 2 from table 1, project column 2 from table 2
(define (simple-inner-join)
  (println  (analyze 3 '(("1" "Q") ("3" "R")) '(("A" "1" "2")("B" "3" "4")) '(("A" "Q")("B" "R"))
  )))

; test simple boolean expression - Return expression Col1 < Col2
(define (simple-boolean-expr1)
   (println  (analyze 3 '((#t) (#f)) '(("A" 5 7)("B" 3 4))
  )))

; test simple date math - Return Expression: Col1 > date constant. 
(define (simple-date-math)
   (println  (analyze 3 '((#t) (#f)) '((new date-fields% [days 29] [months 1] [years 2001])(new date-fields% [days 28] [months 1] [years 2001]))
  )))

; test simple boolean expression - Return Col2 if Col1 is "C" if not return 0
(define (simple-boolean-expr2)
     (println  (analyze 3 '((22) (0)) '(("C" 22)("B" 32))
  )))

; test simple multiply
(define (simple-multiply)
   (println (analyze 3 '((14.0) (18.0)) '((1.4 10.0)(1.8 10.0))
)))

; test simple logical AND, and IF - Return Col3 if Col1 = "C" and Col2 != null, else return 0
(define (simple-logical-and)
   (println (analyze 3 '((0) (14)) '(("C" '() 23)("C" 4 14))
)))

; test max AND IF - Return Max(Col2 - Col3) if Col1 is "C" else return 0
(define (and-if-max)
   (println (analyze 3 '((1) (0) (0)) '(("C" 22 21)("C" 3 22)("B" 20 21))
)))

; test multiply by a constant - Col1 multiplied by .1
(define (constant-multiply)
   (println (analyze 3 '((3.2) (2.0)) '((32)(20))
)))

; test nested Ifs with numerical operations.  If (Col1 is "F" then Col2 else 0) else if (Col2 is "D" then Col 3*Col4 + Col5 else 0) 
; else if (Col1 is "E" then Col6 + Col7 + Col8 + Col5) else 0
(define (nested-if-numerical-ops)
   (println (analyze 3 '((3) (15) (23) (0)) '(("F" 3 3 2 4 10 11 12)("D" 3 5 3 6 6 7 8)("E" 1 2 3 4 5 7 7)("G" 1 2 3 4 5 7 7))
)))

; test if null - If Col1 is null then project Col2, else project Col 3
(define (if-null)
     (println (analyze 3 '((3) (2)) '(('' 3 2)(44 5 2))
)))

; test simple division1 - Col1/(Col 2 - 1)
(define (simple-division1)
     (println (analyze 3 '((1.5) (2.5)) '((3 3)(5 3))
)))

; test simple division2 - (Col1 + Col2)/Col3
(define (simple-division2)
     (println (analyze 3 '((3) (2.66)) '((3 3 2)(5 3 3))
)))

; test boolean exp3 - AND(Col1 > Col2, Col3 = "C")
(define (simple-boolean-expr3)
   (println  (analyze 3 '((#t) (#f) (#f)) '((5 7 "C")(7 5 "C")(7 5 "D"))
  )))

; test boolean expr4 - Return true if Col1 > some constant real
(define (simple-boolean-expr4)
   (println  (analyze 3 '((#t) (#f)) '((.009)(.011))
  )))

; test multiple conditionals - check the semantics of this.
; Return if (Col1 > Col2 AND Col3 > Col4 AND Col5 is not null AND (Col6 = "C" OR Col6="D")) return true else false.
(define (simple-boolean-expr5)
   (println  (analyze 3 '((#t) (#t) (#f) (#f) (#f)) '((34 32 35 23 "C")(34 32 35 23 "D")(31 32 35 23 "C")(34 32 30 23 "C")(34 32 35 '() "C"))
  )))

; return a string constant if some expression is true - If Col1 > 0 then return "True" else return "False"
(define (simple-if1)
   (println  (analyze 3 '(("True") ("False")) '((+0.01)(-0.01))
  )))

; return a string constant if some expression is true - If Col1 > 0 then return "True" else return "False"
(define (simple-if2)
   (println  (analyze 3 '(("True") ("False")) '((+0.01)(-0.01))
  )))

; return strings based on ranges Return "A" if Col1 < 1000, "B" if Col1 < 500, "C" if Col1 < 200, "D" if Col1 >= 1000
(define (range-check)
   (println  (analyze 3 '(("D") ("A")("B")("C")) '((1000)(999)(499)(199))
  )))