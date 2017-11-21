#lang rosette

(require "parse.rkt")
(require "expression-writer.rkt")

(define (test1)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("if" "A" "==" "B" "then" "C"))))
    (println result)))

(define (test2)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("if" "A" "=" "B" "then" "C"))))
    (println result)))

(define (test3)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("if" "A" "is" "equal" "to" "B" "then" "C"))))
    (println result)))

(define (test4)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("if" "A" "is" "equal" "to" "B" "then" "C"))))
    (println result)))

(define (test5)
  (letrec ((p (apply make-parser '("A" "B" "C" "D")))
           (result (p '("if" "A" "is" "equal" "to" "B" "then" "C" "or" "else" "D"))))
    (println result)))

(define (test6)
  (letrec ((p (apply make-parser '("A" "B" "C" "D")))
           (result (p '("if" "A" "is" "equal" "to" "B" "then" "C" "else" "D"))))
    (println result)))

(define (test7)
  (letrec ((p (apply make-parser '("A" "B" "C" "D")))
           (result (p '("if" "A" "is" "equal" "to" "B" "then" "C" "otherwise" "D"))))
    (println result)))

(define (test8)
   (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F" "G" "H")))
           (result (p '("if" "A" "is" "foo" "then" "B" "otherwise" "0"
                             "+" "if" "A" "is" "bar" "then" "(" "B" "*" "C" ")" "+" "D" "else" "0"
                             "+" "if" "A" "is" "baz" "then" "E" "*" "F" "*" "G" "*" "H"))))
    (println result)))

(define (test9)
   (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F" "G" "H")))
           (result (p '("(" "if" "A" "is" "foo" "then" "B" "otherwise" "0" ")"
                             "+" "(" "if" "A" "is" "bar" "then" "(" "B" "*" "C" ")" "+" "D" "else" "0" ")"
                             "+" "(" "if" "A" "is" "baz" "then" "E" "*" "F" "*" "G" "*" "H" ")"))))
    (println result)))

(define (test10)
    (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F")))
           (result (p '("if" "A" "is" "greater" "than" "B" "and" "C" "is" "greater" "than" "D" 
                             "and" "E" "is" "not" "null" "then" "F" "=" "coo" "or" "F" "=" "roo"))))
    (println result)))

(define (test11)
    (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F")))
           (result (p '("if" "(" "(" "A" "is" "greater" "than" "B" ")" "and" "(" "C" "is" "greater" "than" "D" ")" 
                             "and" "(" "E" "is" "not" "null" ")" ")" "then" "(" "F" "=" "coo" "or" "F" "=" "roo" ")"))))
    (println result)))

(define (test11a)
    (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F")))
           (result (p '("if" "(" "(" "A" ">" "B" ")" "and" "(" "C" ">" "D" ")" 
                             "and" "(" "E" "=" "25" ")" ")" "then" "(" "F" "=" "coo" "or" "F" "=" "roo" ")"))))
    (println result)))

(define (test11b)
    (letrec ((p (apply make-parser '("A" "B" "C" "D" "E" "F")))
           (result (p '("if" "(" "A" ">" "B" ")" "and" "(" "C" ">" "D" ")" 
                             "and" "(" "E" "not" "25" ")" "then" "(" "F" "=" "coo" "or" "F" "=" "roo" ")"))))
    (println result)))

(define (test12)
      (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "is" "greater" "than" "B"))))
    (println result)))

(define (test13)
      (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "is" "equal" "to" "B"))))
    (println result)))

(define (test14)
       (letrec ((p (apply make-parser '("A")))
           (result (p '("if" "A" "<" "500" "then" "t3" "else" "if" "A" "<" "1000" "then" "t4" "else" "if" "A" "<" "1500" "then" "t5" "else" "t6"))))
    (println result)))

(define (test15)
       (letrec ((p (apply make-parser '("terms" "min_servers" "price_per_server")))
           (result (p '("if" "terms" "==" "Committed" "then" "min_servers" "*" "price_per_server" "or" "else" "0"))))
    (println result)))

(define (test16)
       (letrec ((p (apply make-parser '("price_per_server" "server")))
           (result (p '("price_per_server" "grouped" "by" "server"))))
    (println result)))

(define (test17)
  (letrec ((p (apply make-parser '("price_per_server" "min_servers" "terms" "top99" "container_overage")))
           (result (p '("if" "terms" "is" "Committed" "then" "price_per_server" "*" "min_servers"
                          "otherwise" "if" "terms" "is" "Standard" "then" "top99" "*" "min_servers" "plus" "container_overage" "otherwise" "0"))))
          (println result)))

(define (test18)
  (letrec ((p (apply make-parser '("price_per_server" "min_servers" "terms" "top99" "container_overage")))
           (result (p  '("if" "terms" "is" "Committed" "then" "price_per_server" "times" "min_servers" "else" "0" "plus"
                          "if" "terms" "is" "Standard" "then" "top99" "times" "price_per_server" "plus" "container_overage" "else" "0"))))
     (println result)))

(define (test19)
  (letrec ((p (apply make-parser '("price_per_server" "min_servers" "terms" "top99" "container_overage")))
           (result (p '("if" "terms" "is" "Committed" "then" "price_per_server" "times"
                             "min_servers" "otherwise" "0" "otherwise" "if" "terms" "is" "Standard"
                             "then" "top99" "times" "price_per_server" "plus" "container_overage" "otherwise" "0"))))
    (println result)))

(define (test20)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("average" "A" "+" "B"))))
    (println result)))

(define (test21)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("average" "A" "+" "B" "group" "by" "C"))))
    (println result)))

(define (test22)
  (letrec ((p (apply make-parser '("A" "B" "C" "D")))
           (result (p '("average" "of" "A" "+" "B" "group" "by" "C" "+" "D"))))
    (println result)))

(define (test23)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("average" "A" "+" "B" "group" "by" "C" "+" "D"))))
    (println result)))

(define (test23a)
  (letrec ((p (apply make-parser '("A" "B" "C")))
           (result (p '("average" "if" "C" "==" "bad" "then" "A" "+" "B"))))
    (println result)))

(define (test24)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("string" "length" "of" "A"))))
    (println result)))

(define (test25)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("index" "of" "A" "in" "B"))))
    (println result)))

(define (test26)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("extract" "seconds" "from" "A"))))
    (println result)))

(define (test27)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("find" "minutes" "from" "A"))))
    (println result)))

(define (test28)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("retrieve" "days" "from" "A"))))
    (println result)))

(define (test29)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("get" "hours" "from" "A"))))
    (println result)))

(define (test30)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("fetch" "months" "from" "A"))))
    (println result)))

(define (test31)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("grab" "years" "from" "A"))))
    (println result)))

(define (test32)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("grab" "day" "of" "week" "from" "A"))))
    (println result)))

(define (test33)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("fetch" "day" "of" "year" "from" "A"))))
    (println result)))

; substring
(define (test34)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("find" "A" "in" "B"))))
    (println result)))

(define (test35)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("extract" "characters" "from" "A" "starting" "at" "0" "ending" "at" "5"))))
    (println result)))

(define (test36)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("extract" "characters" "from" "A" "starting" "at" "0" "ending" "at" "5"))))
    (println result)))

(define (test37)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("append" "A" "to" "B"))))
    (println result)))

; does not work.  suspect keywords for multiply/divide etc are broken in the parser
(define (test39)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("quotient" "of" "A" "divided" "by" "B"))))
    (println result)))

(define (test40)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("remainder" "of" "A" "/" "B"))))
    (println result)))

(define (test41)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("ceiling" "of" "A" "/" "B"))))
    (println result)))

(define (test42)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("floor" "of" "A" "/" "B"))))
    (println result)))

(define (test43)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("absolute" "value" "of" "A" "/" "B"))))
    (println result)))

(define (test44)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("sign" "of" "A" "/" "B"))))
    (println result)))

(define (test45)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("truncate" "A" "/" "B"))))
    (println result)))

(define (test46)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("logarithm" "of" "A" "/" "B"))))
    (println result)))

(define (test47)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("exponent" "of" "A" "*" "B"))))
    (println result)))

(define (test48)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("square" "root" "of" "A" "*" "B"))))
    (println result)))

(define (test49)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("current" "time"))))
    (println result)))

(define (test50)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("remove" "extra" "white" "spaces" "from" "A"))))
    (println result)))

(define (test51)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("mean" "of" "A"))))
    (println result)))

(define (test52)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("count" "of" "A"))))
    (println result)))

(define (test53)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("sum" "A"))))
    (println result)))

(define (test54)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("maximum" "of" "A"))))
    (println result)))

(define (test55)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("minimum" "of" "A"))))
    (println result)))

(define (test56)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "seconds" "to" "A"))))
    (println result)))

(define (test57)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "minutes" "to" "A"))))
    (println result)))

(define (test58)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "hours" "to" "A"))))
    (println result)))

(define (test59)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "days" "to" "A"))))
    (println result)))

(define (test60)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "months" "to" "A"))))
    (println result)))

(define (test61)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "years" "to" "A"))))
    (println result)))


(define (test62)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "seconds" "from" "A"))))
    (println result)))

(define (test63)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "minutes" "from" "A"))))
    (println result)))

(define (test64)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "hours" "from" "A"))))
    (println result)))

(define (test65)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "days" "from" "A"))))
    (println result)))

(define (test66)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "months" "from" "A"))))
    (println result)))

(define (test67)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("subtract" "years" "from" "A"))))
    (println result)))


(define (test68)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "divided" "by" "B"))))
    (println result)))

(define (test69)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("average" "A" "group" "by" "B")))
           (columnMetadata '((columnName "A" primitiveTypes (1)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test70)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("to" "upper" "case" "A")))
           (columnMetadata '((columnName "A" primitiveTypes (3)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test71)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("to" "upper" "case" "(" "A" ")")))
           (columnMetadata '((columnName "A" primitiveTypes (3)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test72)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("extract" "seconds" "from" "A")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test73)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("index" "of" "foo" "in" "A")))
           (columnMetadata '((columnName "A" primitiveTypes (3)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test74)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("add" "10" "seconds" "to" "A")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test75)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("like" "(" "A" "," "B" ")")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))
           
(define (test76)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("values" "in" "(" "A" "," "B" ")")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test77)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("member" "of" "A" "in" "B")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test78)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "like" "B")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test79)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "is" "null")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test80)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("A" "is" "not" "blank")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))

(define (test81)
  (letrec ((p (apply make-parser '("A" "B")))
           (result (p '("concat" "A" "and" "B")))
           (columnMetadata '((columnName "A" primitiveTypes (2)) (columnName "B" primitiveTypes (1)))))
    (println result)
    (println (to-html (car result) columnMetadata))
    (println (jsonify (car result) columnMetadata))))
