#lang rosette

(require "custom.rkt")
(require "expression-lexer.rkt")

(define-symbolic s1 string?)

(define-symbolic i1 integer?)
(define-symbolic i2 integer?)
(define-symbolic i3 integer?)
(define-symbolic i4 integer?)
(define-symbolic i5 integer?)

(define (test1)
  (let-values ([(fs controls) (test-custom '("i2" "*" "i3") (list s1 i2 i3))])
    (println fs)
    (let ((result (solve (assert (and (> i2 1) (< i2 15) (> i3 1) (< i3 15) (= 15 (cdar fs)))))))
      (println result)
      (assert (sat? result))
      (assert (= 15 (evaluate (* i2 i3) result))))))

(define (test2)
  (let-values ([(fs controls) (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3))])
    (let ((result (solve (assert (and (> i2 1) (< i2 17) (> i3 1) (< i3 17) (equal? s1 "bad") (= 17 (cdar fs)))))))
      (println result)
      (println controls)
      (assert (sat? result))
      (assert (= 17 (evaluate (+ i2 i3) result))))))
  
(define (test3)
  (let-values ([(fs controls) (test-custom '("if" "s1" "==" "bad" "then" "i2" "+" "i3") (list s1 i2 i3))])
    (letrec ((test (lambda (guards ctrls)
                     (if (null? ctrls)                         
                         (let ((result (solve (assert (and guards (= (cdar fs) 17))))))
                           (assert (sat? result))
                           (list result))
                         (append
                          (test (and (car ctrls) guards) (cdr ctrls))
                          (test (and (not (car ctrls)) guards) (cdr ctrls)))))))
      (test #t controls))))
                          

(define (test4)
  (analyze-custom '("if" "s1" "==" "bad" "then" "i2" "*" "i3") '(50 0) (list s1 i2 i3) '("bad" 5 10) '("good" 5 10)))

(define (test5)
  (analyze-custom '("if" "s1" "==" "bad" "," "i2" "*" "i3") '(40 50 0) (list s1 i2 i3) '("bad" 4 10) '("bad" 5 10) '("good" 5 10)))

(define (test6)
   (analyze-custom '("s1" "==" "Committed" "and" "i1" ">=" 25 "," "i3" "-" "(" "i4" "*" "i5" ")")
                   '(5000 0 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 10 10000 100 50) '("Custom" 25 10000 100 50)))

(define (test7)
   (analyze-custom '("if" "s1" "==" "Committed" "and" "i1" ">=" 25 "," "i3" "-" "(" "i4" "*" "i5" ")")
                   '(5000 6000 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 25 10000 100 40) '("Custom" 25 10000 100 50)))

(define (test8)
   (analyze-custom '("if" "s1" "==" "Committed" "and" "i1" ">=" 25 "then" "i3" "-" "(" "i4" "*" "i5" ")")
                   '(5000 0 0)
                   (list s1 i1 i3 i4 i5)
                   '("Committed" 25 10000 100 50) '("Committed" 0 10000 100 50) '("Custom" 23 10000 100 50)))

(define (test10)
  (let-values ([(fs controls)
                (test-custom
                 '("if" "s1" "==" "bad" "then" "i2" "+" "i3" "else"
                        "if" "s1" "==" "good" "then" "i2" "else"
                        "if" "i3" ">" "i2" "then" "i3")
                 (list s1 i2 i3))])
    (let ((models (generate-models (cdar fs) controls #t)))
      (println models)
      (assert (>= (length models) 4)))))

(define (test11)
  (let-values ([(fs controls) (test-custom '("if" "s1" "is" "foo" "then" "i1" "otherwise" 0) (list s1 i1))])
    (let ((models (generate-models (cdar fs) controls #t)))
      (assert (>= (length models) 1)))))

(define (test12)
  (let-values ([(fs controls) (test-custom '("if" "s1" "is" "foo" "then" "i1" "otherwise" 0) (list s1 i1))])
    (let ((models (generate-models (cdar fs) controls #t)))
      (assert (>= (length models) 2)))))

(define-symbolic A string?)
(define-symbolic B integer?)
(define-symbolic C integer?)
(define-symbolic D integer?)
(define-symbolic E integer?)
(define-symbolic F integer?)
(define-symbolic G integer?)
(define-symbolic H integer?)

(define (test13)
  (let-values ([(fs controls)
                (test-custom
                 '("if" "A" "is" "foo" "then" "B" "otherwise" "0"
                        "+" "if" "A" "is" "bar" "then" "(" "B" "*" "C" ")" "+" "D" "else" "0"
                             "+" "if" "A" "is" "baz" "then" "E" "*" "F" "*" "G" "*" "H")
                 (list A B C D E F G H))])
    (for/list ([f fs])
      (list (car f) (generate-models (cdr f) controls #t)))))

(define (test13a)
  (parse-generate-data (lex (open-input-string "if A is foo then B otherwise 0 + if A is bar then (B*C)+D else 0 + if A is baz then E*F*G*H"))
                       (list A B C D E F G H)))

(define-symbolic terms string?)
(define-symbolic price_per_server integer?)
(define-symbolic min_servers integer?)

(define (test13b)
    (parse-generate-data (lex (open-input-string "if terms = Committed then price_per_server else 0 + if terms = Standard then price_per_server * min_servers else 0"))
                       (list terms price_per_server min_servers)))

(define (test14)
   (analyze-custom '("if" "A" "is" "foo" "then" "B" "otherwise" "0"
                        "+" "if" "A" "is" "bar" "then" "(" "B" "*" "C" ")" "+" "D" "else" 0
                             "+" "if" "A" "is" "baz" "then" "E" "*" "F" "*" "G" "*" "H")
                   '(0 1 0 -1 0 -1)
                   (list A B C D E F G H)
                   '("foo" 0 0 0 0 0 0 0) '("foo" 1 0 0 0 0 0 0)
                   '("bar" 0 0 0 0 0 0 0) '("bar" 0 0 -1 0 0 0 0)
                   '("baz" 0 0 0 0 0 0 0) '("baz" 0 0 0 1 1 1 -1)))