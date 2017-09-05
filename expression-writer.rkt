#lang rosette

(require json)
(require "interp-enumerate.rkt")

; array of mappings functions to expression operators, and their types
(define mappings (make-hash))

(hash-set! mappings 'or '("or" "BinaryExpression"))
(hash-set! mappings 'and '("and" "BinaryExpression"))
(hash-set! mappings 'not '("not" "UnaryExpression"))
(hash-set! mappings '= '("eq" "BinaryExpression"))
(hash-set! mappings '<= '("leq" "BinaryExpression"))
(hash-set! mappings '>= '("geq" "BinaryExpression"))
(hash-set! mappings '< '("lt" "BinaryExpression"))
(hash-set! mappings '> '("gt" "BinaryExpression"))
(hash-set! mappings 'concat '("concat" "BinaryExpression"))
(hash-set! mappings '+ '("plus" "BinaryExpression"))
(hash-set! mappings '- '("minus" "BinaryExpression"))
(hash-set! mappings '/ '("divide" "BinaryExpression"))
(hash-set! mappings '* '("multiply" "BinaryExpression"))
(hash-set! mappings 'quotient '("quotient" "BinaryExpression"))
(hash-set! mappings 'remainder '("modulo" "BinaryExpression"))
(hash-set! mappings 'abs '("absolute" "UnaryExpression"))
(hash-set! mappings 'ceiling '("ceiling" "UnaryExpression"))
(hash-set! mappings 'floor '("floor" "UnaryExpression"))
(hash-set! mappings 'truncate '("truncate" "UnaryExpression"))
(hash-set! mappings 'sign '("sign" "UnaryExpression"))
(hash-set! mappings 'if '("ifelse" "TernaryExpression"))
(hash-set! mappings 'substring '("substring" "BinaryExpression"))
(hash-set! mappings 'index-of '("index-of" "BinaryExpression"))
(hash-set! mappings 'length '("string-length" "UnaryExpression"))
(hash-set! mappings '== '("eq" "BinaryExpression"))
(hash-set! mappings 'in '("column" "Column"))
(hash-set! mappings 'is-null '("isnull" "UnaryExpression"))
(hash-set! mappings 'is-not-null '("is-not-null" "UnaryExpression"))



; define a set of mappings from functions to their pretty equivalents
(define pretty (make-hash))
(hash-set! pretty 'or "lhs or rhs")
(hash-set! pretty 'and "lhs and rhs")
(hash-set! pretty 'not "is not expression")
(hash-set! pretty '= "lhs = rhs")
(hash-set! pretty '<= "lhs <= rhs")
(hash-set! pretty '>= "lhs >= rhs")
(hash-set! pretty '< "(lhs < rhs)")
(hash-set! pretty '> "lhs > rhs")
(hash-set! pretty 'concat "lhs concatenated with rhs")
(hash-set! pretty '+ "lhs + rhs")
(hash-set! pretty '- "(lhs - rhs)")
(hash-set! pretty '/ "lhs / rhs")
(hash-set! pretty '* "lhs * rhs")
(hash-set! pretty 'quotient "'quotient when lhs / rhs" )
(hash-set! pretty 'remainder "<remainder when lhs / rhs")
(hash-set! pretty 'abs "absolute value of expression")
(hash-set! pretty 'ceiling "ceiling of expression")
(hash-set! pretty 'floor "floor of expression")
(hash-set! pretty 'truncate "truncated expression")
(hash-set! pretty 'sign "sign of expression")
(hash-set! pretty 'if "if one is true then two otherwise three")
(hash-set! pretty 'substring "a part of one from location two to location three")
(hash-set! pretty 'index-of "location of rhs in lhs")
(hash-set! pretty 'length "length of expression")
(hash-set! pretty '== "lhs = rhs")
(hash-set! pretty 'in "expression")
(hash-set! pretty 'is-null "expression is null")
(hash-set! pretty 'is-not-null "expression is not null")

(define (check-exp type)
  (if (hash-has-key? mappings type)
      (hash-ref mappings type)
      (raise "No expression found")))

(define (create-value-exp n)
  (list 'objectType "ValueExpression" 'operator "value" 'value n))
               
; get the output of render and rewrite it as json - assume we will only write the first solution
(define (jsonify tree column-metadata)
  (letrec ((print-tree
            (lambda (node)
              (if (list? node)
                  (letrec
                      ((exp (check-exp (list-ref node 0)))
                       (base (list 'objectType (list-ref exp 1) 'operator (list-ref exp 0))))
                    (cond [(equal? "Column" (list-ref exp 1))
                           ; if expression is equal to column, then pull some metadata of the column
                           ; directly off the column data that was posted to the server.
                           ; Need code to understand what that data structure should be
                           (apply hasheq (append base (list-ref column-metadata (- (list-ref node 1) 1))))]
                          [(equal? "UnaryExpression" (list-ref exp 1))
                           (apply hasheq (append base (list 'expression (print-tree (cadr node)))))]
                          [(equal? "BinaryExpression" (list-ref exp 1))
                           (apply hasheq (append base (list 'lhs (print-tree (list-ref node 1)) 'rhs (print-tree (list-ref node 2)))))]
                          [(equal? "TernaryExpression" (list-ref exp 1))
                           (apply hasheq (append base (list 'one (print-tree (list-ref node 1)) 'two (print-tree (list-ref node 2)) 'three (print-tree (list-ref node 3)))))]))
                  (apply hasheq (create-value-exp node))))))
    (print-tree tree)))

(define (handle-args l replaceArg text f)
  (for/fold ([t text])
            ([i (in-range 1 (length l))]
             [j replaceArg])
    (string-replace t j (f (list-ref l i)) #:all? #f)))

(define (handle l replaceArg text f)
  (let ((txt (handle-args l replaceArg text f)))
    (for/fold ([t txt])
              ([i replaceArg])
      (string-replace t i "&lt; insert formula part here &gt;" #:all? #f))))


; create an HTML text around this solution.  Add spans for functions so that they can get turned into 'black lists'
(define (to-html tree col-metadata)
  (letrec ((print-tree
            (lambda (node)
              (if (list? node)
                  (let ((text (hash-ref pretty (list-ref node 0)))
                        (exp (check-exp (list-ref node 0))))
                    (cond [(equal? "Column" (list-ref exp 1))
                           ; if expression is equal to column, then pull some metadata of the column
                           ; directly off the column data that was posted to the server.
                           (let ((colAttrs (apply hasheq (list-ref col-metadata (- (list-ref node 1) 1)))))
                             (string-replace text "expression" (hash-ref colAttrs 'columnName) #:all? #f))]
                          [(equal? "UnaryExpression" (list-ref exp 1))
                           (string-append "(" (handle node '("expression") text print-tree) ")")]
                          [(equal? "BinaryExpression" (list-ref exp 1))
                             (string-append "(" (handle node '("lhs" "rhs") text print-tree) ")")]
                          [(equal? "TernaryExpression" (list-ref exp 1))
                             (string-append "(" (handle node '("one" "two" "three") text print-tree) ")")]))
                  (string-append (~a node))))))
    (print-tree tree)))


(provide to-html jsonify)
