#lang rosette

(require "./interp-enumerate.rkt")
; Need the following sets of features:
; 1. Ability to get a different solution if this one does not work
; 2. Ability to deal with reals properly
; 3. Add logical not to the sets of operations we need
; 4. Add a solution to: solution: unrecognized solver output: (error line 7404 column 10: model is not available)
; 5. Need to extend to more columns
; 6. Fix division by zero problem with simple-multiply2
; 7. fix assertions
; 8. if then else wont work if we have multiple conditions (case statement for a whole set of conds



; test selection of a certain column (Col2) based on value in a different column (Col1)
(define (simple-selection1)
  (test analyze '< 5 '(5 0) '(("A" 5)("B" 7))))  

; test a simple multiply - need to handle reals properly because of precision issues
(define (simple-multiply1)
  (test analyze '('*) 5 '(2.4 2.469) '((4 .6)(8.23 .3))))

; test a simple multiply - by a constant
(define (simple-multiply2)
  (test analyze '* 5 '(.008 .016) '((4)(8)))) 

; test combination of ANDs, NOTs, and arithmetic operations
(define (simple-test1)
  (test analyze '* 5 '(2.6 0 0) '(("A" "FOO" 5 4 .6)("A" "G" 4 8.23 .3)("B" "G" 4 8.23 .3))))

; if col1="" then take col3 else col2
(define (simple-test2)
   (test analyze '< 5 '(5 0) '(("A" 5)("" 7))))

; define (col1 + col2) / col3
(define (simple-test3)
   (test analyze '< 5 '(3.55 3.85) '((4.3 2.8 2)(3.5 4.2 2))))

; and (col1 > col2, col3 = "A")
(define (simple-test4)
  (test analyze '('AND '> '=) 5 '(#t #f #f #f) '((5 3 "A")(3 5 "A")(5 3 "B")(5 5 "A"))))

; test if col1 > .33
(define (simple-test5)
  (test analyze '('AND '> '=) 5 '(#t #t #f) '((.44)(.34)(.33))))

; test if (col1 > col2) and (col2 < col3) and (col4 = "C" or col4 = "D")
(define (simple-test6)
  (test analyze '('AND '> '=) 5 '(#t #f #f #t #f) '((5 2 4 "C")(5 2 1 "C")(1 2 4 "C")(8 1 5 "D")(5 2 4 "M"))))

; test simple boolean expression - Return expression Col1 < Col2
(define (simple-boolean1)
(test analyze '< 5 '(#t #f #f) '(("A" 5 7)("B" 7 5)("C" 5 5))))

