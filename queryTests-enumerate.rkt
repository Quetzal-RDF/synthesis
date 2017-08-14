#lang rosette

(require "./interp-enumerate.rkt")
; Need the following sets of features:
; 1. Ability to get a different solution if this one does not work
; 2. Ability to deal with reals properly
; 4. Add a solution to: solution: unrecognized solver output: (error line 7404 column 10: model is not available)
; 5. Need to extend to more columns
; 6. Fix division by zero problem with simple-multiply2
; 8. if then else wont work if we have multiple conditions (case statement for a whole set of conds

(define-symbolic i1 integer?)
(define-symbolic i2 integer?)
(define-symbolic i3 integer?)
(define-symbolic i4 integer?)
(define-symbolic i5 integer?)
(define-symbolic s1 string?)
(define-symbolic s2 string?)
(define-symbolic r1 real?)
(define-symbolic r2 real?)

(define mult234
  (custom
   (lambda (p pos)
     (send p basic-math pos
           (send p basic-math (cons 1 pos)
                 (send p in-v pos 2 number?)
                 (send p in-v pos 3 number?))
           (send p in-v pos 4 number?)))))

(define extra-m234 (hash 'number (list mult234)))

(define mult23x
  (custom
   (lambda (p pos v)
     (send p basic-math pos
           (send p basic-math (cons 1 pos)
                 (send p in-v pos 2 number?)
                 (send p in-v pos 3 number?))
           v))
   do-all-int))

(define extra-m23x (hash 'number (list mult23x)))

; test selection of a certain column (Col2) based on value in a different column (Col1)
(define (simple-selection1)
  (test analyze '(if) '() '(index-of) 3 '(5 0) (list s1 i1) '(("A" 5)("B" 5))))

(define (simple-selection1a)
  (test analyze '(if) '() '() 3 '(1050 0) (list s1 i1) '(("Committed" 1050)("Custom" 1050))))

; test selection of a certain column based on its string
(define (simple-selection2)
  (test analyze '() '() '() 3 '(1050 0) (list s1 i1) '(("Committed" 1050) ("Custom" 1050))))

; test multiplication of 2 columns based on string in the first column - return Col 2 * Col 3 if Col1 = "Committed"
(define (simple-selection3)
  (test analyze '() '() '() 3 '(50000 0 1000) (list s1 i1 i2) '(("Committed" 1000 50) ("Custom" 1000 10) ("Committed" 100 10))))

(define math345
  (custom
   (lambda (p pos)
     (send p basic-math pos
           (send p in-v pos 3 number?)
           (send p basic-math (cons 1 pos)
                 (send p in-v pos 4 number?)
                 (send p in-v pos 5 number?))))))

(define committed-if
  (custom
   (lambda (p pos yes no)
     (send p if-then-else
           (send p logic-op (cons 3 pos)
                 (send p compare-to-str (cons 4 pos)
                       (send p in-v (cons 5 pos) 1 string?)
                       (send p symbolic (cons 6 pos) string?))
                 (send p is-null? (cons 7 pos)))
           yes no))
   do-all-int do-all-int))

(define committed
  (custom
   (lambda (p pos)
     (send p logic-op (cons 3 pos)
           (send p compare-to-str (cons 4 pos)
                 (send p in-v (cons 5 pos) 1 string?)
                 (send p symbolic (cons 6 pos) string?))
           (send p is-null? (cons 7 pos))))))

(define extra-selection4 (hash 'boolean (list committed) 'number (list math345)))

; test selection of a certain column (Col2) based on value in a different column (Col1)
(define (simple-selection1b)
  (test analyze '(if) '() '() 3 '(1050 0) (list s1 i1) '(("Committed" 1050)("Custom" 1050))))  

; test a simple multiply - need to handle reals properly because of precision issues
(define (simple-multiply1)
  (test analyze '(*) '() '() 3 '(2.4 2.469) (list r1 r2) '((4 .6)(8.23 .3))))

; test simple boolean expression - Return expression Col2 < Col3
(define (simple-boolean1)
  (test analyze '(>) '() '() 5 '(#t #f #f #t) (list s1 i1 i2) '(("A" 5 7)("B" 7 5)("C" 5 5)("D" 1 2))))

; test simple boolean expression - Return expression Col3 > Col2 and Col1 == "A"
(define (simple-boolean2)
  (test analyze '(==) '() '(index-of substring) 5 '(#t #f #f #f) (list s1 i1 i2) '(("A" 5 7)("A" 7 5)("C" 7 5)("C" 5 7))))

; define (col1 + col2) / col3
(define (simple-math1)
  (test analyze '(+) '() '(if) 5 '(2 4 4) (list i1 i2 i3) '((1 1 1)(9 7 4)(3 5 2))))

; if col1="A" then take col2 else 0
(define (simple-eq1)
  (test analyze '(if ==) '() '() 5 '(5 0 13) (list s1 i1) '(("A" 5)("" 7)("A" 13))))

; and (col1 > col2, col3 = "A")
(define (simple-compare1)
  (test analyze '(==) '() '(index-of substring) 5 '(#t #f #f #f) (list i1 i2 s1) '((5 3 "A")(3 5 "A")(5 3 "B")(5 5 "A"))))

; test if col1 > .33
(define (simple-test5)
  (test analyze '(<=) '() '() 5 '(#t #t #f) (list r1) '((.44)(.34)(.33))))

; test a simple multiply - by a constant
(define (simple-multiply2)
  (test analyze '(/) '() '() 3 '(.008 .016) (list i1 i2) '((4)(8)))) 

; test string equal
(define (simple-compare2)
  (test analyze '(==) '() '(index-of substring abs) 6 '(9 0 15) (list i1 i2 s1) '((4 3 "A") (4 3 "B") (5 5 "A"))))

; if col1="" then take col3 else col2
(define (simple-test2)
  (test analyze '(==) '() '(abs index-of) 5 '(5 0) (list s1 i1) '(("A" 5)("" 5))))

; define (col1 + col2) / col3
(define (simple-test3)
  (test analyze '(/) '() '() 3 '(3.55 3.85) (list r1 r2 i1) '((4.3 2.8 2)(3.5 4.2 2))))

; and (col1 > col2, col3 = "A")
(define (simple-test4)
  (test analyze '(<) '() '(substring) 5 '(#t #f #f #f) (list i1 i2 s1) '((5 3 "A")(3 5 "A")(5 3 "B")(5 5 "A"))))



; test if (col1 > col2) and (col2 < col3) and (col4 = "C" or col4 = "D")
(define (simple-test6)
  (test analyze '(>) '() '(index-of) 5 '(#t #f #f #t #f) (list i1 i2 i3 s1) '((5 2 4 "C")(5 2 1 "C")(1 2 4 "C")(8 1 5 "D")(5 2 4 "M"))))

; test if Col1 is "Committed" AND Col2 is not blank, then max(0, Col3 - (Col4 * Col5))
(define (simple-selection4)
  (test analyze '() '() '(substring) 5 '(5000 0 0) (list s1 i1 i2 i3 i4) '(("Committed" 25 10000 100 50) ("Committed" () 10000 100 50) ("Custom" 25 10000 100 50))))



; test combination of ANDs, NOTs, and arithmetic operations
(define (simple-test1)
  (test analyze '(*) '() '() 3 '(2.6 0 0) (list s1 s2 i1 i2 r1) '(("A" "FOO" 5 4 .6)("A" "G" 10 9.7 1.3)("B" "G" 4 1.23 .3))))
  
