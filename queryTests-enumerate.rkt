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
(define-symbolic i6 integer?)
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
  (test analyze '(if) '() '(index-of) 7 '(5 0) (list s1 i1) '(("A" 5)("B" 5))))

(define (simple-symbolic1)
  (test analyze '(if) '() '() 7 '(5 0) (list s1 i1) (list (list "A" i2) (list "B" 5))))

(define (simple-symbolic2)
  (test analyze '(if) '() '() 7 (list i2 0) (list s1 i1) (list (list "A" i2) (list "B" 5))))

(define (simple-symbolic3)
  (test analyze '() '() '() 7 (list s2 "B") (list s1 i1) (list (list s2 1) (list "B" 5))))

; test selection of a certain column based on its string
(define (simple-selection2)
  (test analyze '() '() '() 7 '(1050 0) (list s1 i1) '(("Committed" 1050) ("Custom" 1050))))

; test multiplication of 2 columns based on string in the first column - return Col 2 * Col 3 if Col1 = "Committed"
(define (simple-selection3)
  (test analyze '() '() '() 10 '(50000 0 1000) (list s1 i1 i2) '(("Committed" 1000 50) ("Custom" 1000 10) ("Committed" 100 10))))

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
  (test analyze '(if) '() '() 7 '(1050 0) (list s1 i1) '(("Committed" 1050)("Custom" 1050))))  

; test a simple multiply - need to handle reals properly because of precision issues
(define (simple-multiply1)
  (test analyze '(*) '() '() 3 '(2.4 2.469) (list r1 r2) '((4 .6)(8.23 .3))))

; test simple boolean expression - Return expression Col2 < Col3
(define (simple-boolean1)
  (test analyze '(>) '() '() 5 '(#t #f #f #t) (list s1 i1 i2) '(("A" 5 7)("B" 7 5)("C" 5 5)("D" 1 2))))

; test simple boolean expression - Return expression Col3 > Col2 and Col1 == "A"
(define (simple-boolean2)
  (test analyze '(==) '() '(index-of substring) 9 '(#t #f #f #f) (list s1 i1 i2) '(("A" 5 7)("A" 7 5)("C" 7 5)("C" 5 7))))

; define (col1 + col2) / col3
(define (simple-math1)
  (test analyze '(+) '() '() 6 '(2 4 5) (list i1 i2 i3) '((1 1 1)(9 7 4)(4 6 2))))

; if col1="A" then take col2 else 0
(define (simple-eq1)
  (test analyze '(if ==) '() '(index-of length) 9 '(5 0 13) (list s1 i1) '(("A" 5)("AAA" 7)("A" 13))))

; and (col1 > col2, col3 = "A")
(define (simple-compare1)
  (test analyze '(==) '() '(index-of substring) 9 '(#t #f #f #f) (list i1 i2 s1) '((5 3 "A")(3 5 "A")(5 3 "B")(5 5 "A"))))

; test if col1 > .33
(define (simple-test5)
  (test analyze '(<=) '() '() 7 '(#t #t #f) (list r1) '((.44)(.34)(.33))))

; test a simple multiply - by a constant
(define (simple-multiply2)
  (test analyze '(/) '() '() 3 '(.008 .016) (list i1 i2) '((4)(8)))) 

; test string equal
(define (simple-compare2)
  (test analyze '(==) '() '(index-of substring abs) 10 '(9 0 15) (list i1 i2 s1) '((4 3 "A") (4 3 "B") (5 5 "A"))))

; if col1="" then take col3 else col2
(define (simple-test2)
  (test analyze '(==) '() '(abs index-of length) 7 '(5 0) (list s1 i1) '(("A" 5)("" 5))))

; define (col1 + col2) / col3
(define (simple-test3)
  (test analyze '(/) '() '() 6 '(3.55 3.85) (list r1 r2 i1) '((4.3 2.8 2)(3.5 4.2 2))))

; and (col1 > col2, col3 = "A")
(define (simple-test4)
  (test analyze '(and) '() '(substring) 9 '(#t #f #f #f) (list i1 i2 s1) '((5 3 "A")(3 5 "A")(5 3 "B")(5 5 "A"))))



; test if (col1 > col2) and (col2 < col3) and (col4 = "C" or col4 = "D")
(define (simple-test6)
  (test analyze '(>) '() '(index-of) 5 '(#t #f #f #t #f) (list i1 i2 i3 s1) '((5 2 4 "C")(5 2 1 "C")(1 2 4 "C")(8 1 5 "D")(5 2 4 "M"))))

; test if Col1 is "Committed" AND Col2 is not blank, then max(0, Col3 - (Col4 * Col5))
(define (simple-selection4)
  (test analyze '() '() '(substring) 5 '(5000 0 0) (list s1 i1 i2 i3 i4) '(("Committed" 25 10000 100 50) ("Committed" () 10000 100 50) ("Custom" 25 10000 100 50))))


; something wrong with strings...
(define (simple-selection1c)
  (test analyze '(if) '() '(index-of) 3 '("5" "0") (list s1 s2) '(("A" "5")("B" "5"))))

; test combination of ANDs, NOTs, and arithmetic operations
(define (simple-test1)
  (test analyze '(*) '() '() 10 '(2.6 0 0) (list s1 s2 i1 i2 r1) '(("A" "FOO" 5 4 .6)("A" "G" 10 9.7 1.3)("B" "G" 4 1.23 .3))))

(define v #(i1 i2 i3 i4 i5 i6))

; add a second to date
(define (simple-date-add1)
  (test analyze '() '() '() 10 (list #(0 0 0 29 2 2000) #(23 59 23 28 2 2000) #(0 46 23 28 2 1999)) (list v) (list (list #(59 59 23 28 2 2000)) (list #(22 59 23 28 2 2000)) (list #(59 45 23 28 2 1999)))))

; add a minute to date
(define (simple-date-add2)
  (test analyze '() '() '() 10 (list #(59 0 0 1 3 2001)  #(22 23 23 28 2 2000) #(59 0 0 1 1 2000)) (list v) (list (list #(59 59 23 28 2 2001)) (list #(22 22 23 28 2 2000)) (list #(59 59 23 31 12 1999)))))

; add hour to date
(define (simple-date-add3)
  (test analyze '() '() '() 10 (list #(59 59 0 1 3 2001)  #(22 22 0 29 2 2000)  #(59 59 0 1 1 2000)) (list v) (list (list #(59 59 23 28 2 2001)) (list #(22 22 23 28 2 2000)) (list #(59 59 23 31 12 1999)))))

; add days to date
(define (simple-date-add4)
  (test analyze '() '() '() 10 (list #(59 59 23 2 3 2001)  #(22 22 23 27 2 2000)  #(59 59 23 2 1 2000)) (list v) (list (list #(59 59 23 28 2 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; add months to date
(define (simple-date-add5)
  (test analyze '() '() '() 10 (list #(59 59 23 28 2 2001)  #(22 22 23 25 3 2000)  #(59 59 23 31 1 2000)) (list v) (list (list #(59 59 23 30 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; add years to date
(define (simple-date-add6)
  (test analyze '() '() '() 10 (list #(59 59 23 30 1 2016)  #(22 22 23 25 2  2015)  #(59 59 23 31 12 2014)) (list v) (list (list #(59 59 23 30 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; subtract seconds from date
(define (simple-date-subtract1)
  (test analyze '() '() '() 10 (list #(59 59 23 31 12 2000)  #(21 22 23 25 2  2000)  #(58 59 23 31 12 1999)) (list v) (list (list #(0 0 0 1 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; subtract 1 minute from date
(define (simple-date-subtract2)
  (test analyze '() '() '() 10 (list #(0 59 23 31 12 2000)  #(22 21 23 25 2  2000)  #(59 58 23 31 12 1999)) (list v) (list (list #(0 0 0 1 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; subtract 1 hour from date
(define (simple-date-subtract3)
  (test analyze '() '() '() 10 (list #(0 0 23 31 12 2000)  #(22 22 22 25 2  2000)  #(59 59 22 31 12 1999)) (list v) (list (list #(0 0 0 1 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; subtract 1 day from date
(define (simple-date-subtract4)
  (test analyze '() '() '() 10 (list #(0 0 0 31 12 2000)  #(22 22 23 24 2  2000)  #(59 59 23 30 12 1999)) (list v) (list (list #(0 0 0 1 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))

; subtract 1 month from date
(define (simple-date-subtract5)
  (test analyze '() '() '() 10 (list #(0 0 0 1 3 2001)  #(59 59 23 30 11 1999)) (list v) (list (list #(0 0 0 1 4 2001)) (list #(59 59 23 31 12 1999)))))

; subtract 10 years from date
(define (simple-date-subtract6)
  (test analyze '() '() '() 10 (list #(0 0 0 1 1 1991)  #(22 22 23 31 3 1990)  #(59 59 23 31 12 1989)) (list v) (list (list #(0 0 0 1 1 2001)) (list #(22 22 23 31 3 2000)) (list #(59 59 23 31 12 1999)))))

; epoch to date
(define (epoch-to-date-test1)
  (test analyze '() '() '() 10 (list #(28 26 18 4 10 2017)  #(0 24 8 4 10 2016)  #(0 0 2 1 1 2016)) (list v) (list (list 1507141588) (list 1475569440) (list 1451613600))))

; epoch to date + add 1 s
(define (epoch-to-date-test2)
  (test analyze '() '() '() 10 (list #(29 26 18 4 10 2017)  #(1 24 8 4 10 2016)  #(1 0 2 1 1 2016)) (list v) (list (list 1507141588) (list 1475569440) (list 1451613600))))

; date extract
(define (date-extract1)
  (test analyze '() '() '() 5 (list 59 22 59 59 59) (list v) (list (list #(59 59 23 28 2 2000)) (list #(22 59 23 28 2 2000))(list #(59 45 23 28 2 2000))(list #(59 45 23 28 2 1999))(list #(59 45 23 28 2 1990)))))

(define (date-extract2)
  (test analyze '() '() '() 5 (list 59 59 45) (list v) (list (list #(59 59 23 28 2 2000)) (list #(22 59 23 28 2 2000))(list #(59 45 23 28 2 2000)))))

(define (date-extract3)
  (test analyze '() '() '() 5 (list 22 23 21) (list v) (list (list #(59 59 22 28 2 2000)) (list #(22 59 23 28 2 2000))(list #(59 45 21 28 2 2000)))))

(define (date-extract4)
  (test analyze '() '() '() 5 (list 28 22 1) (list v) (list (list #(59 59 22 28 2 2000)) (list #(22 59 23 22 2 2000))(list #(59 45 21 1 2 2000)))))

(define (date-extract5)
  (test analyze '() '() '() 5 (list 1 12 8) (list v) (list (list #(59 59 22 28 1 2000)) (list #(22 59 23 22 12 2000))(list #(59 45 21 1 8 2000)))))

(define (date-extract6)
  (test analyze '() '() '() 5 (list 2001 2004 2008) (list v) (list (list #(59 59 22 28 1 2001)) (list #(22 59 23 22 12 2004))(list #(59 45 21 1 8 2008)))))

; diff dates - only works for days as defined by the function
(define (date-diff-test)
    (test analyze '() '() '() 5 (list 1 4 8) (list v) (list (list #(59 59 22 28 1 2000) #(59 59 22 27 1 2000)) (list #(22 59 23 22 12 2000) #(22 59 23 18 12 2000))(list #(59 45 21 10 8 2000) #(59 45 21 2 8 2000)))))

; filter dates that are >= 2000
(define (simple-date-filter1)
  (test analyze '() '() '() 5 (list #t #t #t #f #f) (list v) (list (list #(59 59 23 28 2 2000)) (list #(22 59 23 28 2 2000))(list #(59 45 23 28 2 2000))(list #(59 45 23 28 2 1999))(list #(59 45 23 28 2 1990)))))

; filter dates that are == 2000
(define (simple-date-filter2)
  (test analyze '() '() '() 5 (list #t #t #t #f #f) (list v) (list (list #(59 59 23 28 2 2000)) (list #(22 59 23 28 2 2000))(list #(59 45 23 28 2 2000))(list #(59 45 23 28 2 2002))(list #(59 45 23 28 2 2001)))))

; date to epoch
(define (date-to-epoch-test1)
  (test analyze '() '() '() 10 (list 1507141588 1475569440 1451613600) (list v) (list (list #(28 26 18 4 10 2017))  (list #(0 24 8 4 10 2016))  (list #(0 0 2 1 1 2016)))))


; DOES NOT WORK FUNCTIONS
; compose add years + add seconds
; add years to date
(define (simple-date-add7)
  (test analyze '() '() '() 10 (list #(0 0 0 31 1 2016)  #(23 22 23 25 2 2015)  #(0 0 0 1 1 2015)) (list v) (list (list #(59 59 23 30 1 2001)) (list #(22 22 23 25 2 2000)) (list #(59 59 23 31 12 1999)))))


