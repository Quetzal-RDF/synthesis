#lang rosette

(require rosette/lib/angelic)

(define (make-parser . xcolumn-names)
  (let* ((column-names
          (map (lambda (s) (string-trim s #px"\\$.+")) xcolumn-names))
         (keywords
          '((or ("or") ("either") ("any") ("instead"))
            (is-null ("is" "null") ("is" "empty"))
            (is-not-null ("is" "not" "null") ("is" "not" "empty"))
            (and ("and") ("both") ("also") ("including"))
            (not ("not") ("never") ("neither") ("no") ("don't") ("isn't")("is" "not"))
            (concatenate ("concatenate") ("concat") ("combine") ("transform") ("append") ("merge") ("mix") ("encode"))
            (quotient ("quotient"))
            (remainder ("remainder")("remainder")("modulo")("mod")("%"))
            (abs ("abs") ("absolute") ("magnitude") ("positive"))
            (ceiling ("ceiling") ("round" "up"))
            (floor ("floor") ("round" "down"))
            (truncate ("truncate")("trunc")("drop") ("shorten") ("abbreviate") ("cut"))
            (sign ("sign") ("positive" "or" "negative"))
            (if ("if") ("whether")("when")("unless")("case")("in case")("case" "when"))
            (then ("then") ("do") ("then" "do") ("subsquently"))
            (else ("else") ("or" "else") ("or") ("otherwise"))
            (substring ("substring")("left") ("right") ("mid"))
            (substring-first ("extract") ("extract" "characters") ("extract" "string") ("extract" "substring")("find")("find" "characters")("find" "substring"))
            (substring-start ("starting" "at") ("start" "at"))
            (substring-end ("ending" "at") ("end" "at"))
            (index-of ("index" "of")("position" "of")("find" "index")("find" "index" "of")("find" "position")("find" "position" "of")("search" "position")("search" "position" "of"))
            (length ("length")("string" "length")("len")("length" "of")("string" "length" "of")("character" "length" "of")("number" "of" "characters" "in"))
            (+ ("+") ("plus"))
            (- ("-") ("minus") ("takeaway") ("subtract") ("deduct"))
            (/ ("/") ("divide") ("divided" "by"))
            (* ("*") ("multiply" "by") ("multiply") ("times"))
            (exponent ("exponent") ("exp") ("power"))
            (logarithm ("logarithm") ("log") ("ln") ("natural logarithm"))
            (sqrt ("sqrt") ("square" "root") ("square" "root" "of"))
            (lower ("lower" "to lower case"))
            (upper ("upper" "to upper case"))
            (trim ("trim") ("remove" "spaces") ("remove" "extra" "spaces")("remove" "extra" "white" "spaces")("remove" "white" "spaces") ("pare") ("pare" "spaces")("pare" "extra" "spaces")("pare" "white" "spaces")("pare" "extra" "white" "spaces")("cut" "spaces")("cut" "white" "spaces")("cut" "extra" "white" "spaces"))
            (replace ("replace") ("overlay") ("change"))
            (like ("like") ("matches")("is like"))
            (matches ("matches") ("contains") ("includes") ("has") ("appears in"))
            (now ("now") ("current" "time") ("time" "now") ("today"))
            (extract ("extract") ("retrieve") ("pull" "out") ("grab") ("fetch") ("find") ("obtain") ("get"))
            (> (">") ("after") ("is" "after") ("greater") ("is" "greater" "than") ("greater" "than") ("more") ("more" "than") ("is" "more" "than") ("larger") ("larger" "than") ("is" "larger" "than") ("higher") ("higher" "than") ("is" "higher" "than") ("bigger") ("bigger" "than") ("is" "bigger" "than") ("older") ("older" "than") ("is" "older" "than"))
            (< ("<") ("before") ("is" "before") ("less") ("less" "than") ("is" "less" "than") ("smaller") ("smaller" "than") ("is" "smaller" "than") ("lower") ("lower" "than") ("is" "lower" "than") ("younger") ("younger" "than") ("is" "younger" "than"))
            (>= (">=") ("greater" "than" "or" "equal" "to") ("is" "greater" "than" "or" "equal" "to") ("more" "than" "or" "equal" "to") ("is" "more" "than" "or" "equal" "to") ("larger" "than" "or" "equal" "to") ("is" "larger" "than" "or" "equal" "to") ("higher" "than" "or" "equal" "to") ("is" "higher" "than" "or" "equal" "to") ("bigger" "than" "or" "equal" "to") ("is" "higher" "than" "or" "equal" "to") ("older" "than" "or" "equal" "to") ("is" "older" "than" "or" "equal" "to"))
            (<= ("<=") ("less" "than" "or" "equal" "to") ("is" "less" "than" "or" "equal" "to") ("smaller" "than" "or" "equal" "to") ("is" "smaller" "than" "or" "equal" "to") ("lower" "than" "or" "equal" "to") ("is" "lower" "than" "or" "equal" "to") ("is" "younger" "than" "or" "equal" "to"))
            (!= ("!=") ("<>") ("not equal") ("is" "not" "equal" "to") ("is" "not" "same" "as"))
            (= ("=") ("==") ("equals") ("is") ("is" "equal" "to") ("is" "same" "as"))
            (average ("average") ("mean") ("avg"))
            (minimum ("minimum") ("min") ("lowest") ("lowest" "value"))
            (maximum ("maximum") ("max") ("highest") ("highest" "value"))
            (sum ("sum") ("sum") ("total"))
            (count ("count") ("count" "values")("count" "number" "of" "rows"))
            (concatenate-with ("to") ("with") ("and"))
            (add-seconds ("add" "seconds")("add" "seconds" "to"))
            (add-minutes ("add" "minutes")("add" "minutes" "to"))
            (add-hours ("add" "hours")("add" "hours" "to"))
            (add-days ("add" "days")("add" "days" "to"))
            (add-months ("add" "months")("add" "months" "to"))
            (add-years ("add" "years")("add" "years" "to"))
            (subtract-seconds ("subtract" "seconds")("subtract" "seconds" "from"))
            (subtract-minutes ("subtract" "minutes")("subtract" "minutes" "from"))
            (subtract-hours ("subtract" "hours")("subtract" "hours" "from"))
            (subtract-days ("subtract" "days")("subtract" "days" "from"))
            (subtract-months ("subtract" "months")("subtract" "months" "from"))
            (subtract-years ("subtract" "years")("subtract" "years" "from"))
            (group ("group") ("group" "by") ("grouped" "by") ("organize" "by") ("organized" "by") ("cluster" "by") ("clustered" "by") ("by") ("based" "on"))))
         (reserved (filter string? (flatten keywords)))
         (templates '((is-null "(" () ")")
                      (is-not-null "(" () ")")
                      (index-of () "in" ())
                      (substring "(" () "," () "," () ")")
                      (substring-first "from" () substring-start () substring-end ())
                      (concatenate () concatenate-with ())
                      (concatenate "(" () "," () ")")
                      (replace "(" () "," () "," () ")")
                      (matches "(" () "," () "," () ")")
                      (like "(" () "," () "," () ")")
                      (now)
                      (quotient "(" () ")")
                      (quotient ())
                      (quotient "of" ())
                      (remainder ())
                      (remainder "of" ())
                      (remainder "(" () ")")
                      (abs "of" ())
                      (abs "value" ())
                      (abs "value" "of" ())
                      (abs "(" () ")")
                      (ceiling "of" ())
                      (ceiling ())
                      (ceiling "(" () ")")
                      (floor "of" ())
                      (floor ())
                      (floor "(" () ")")
                      (truncate ())
                      (truncate "(" () ")")
                      (sign "(" () ")")
                      (sign "of" ())
                      (exponent "(" () ")")
                      (exponent "of" ())
                      (logarithm "(" () ")")
                      (logarithm "of" ())
                      (sqrt "(" () ")")
                      (sqrt ())
                      (upper "(" () ")")
                      (lower "(" () ")")
                      (length "(" () ")")
                      (length ())
                      (trim "(" () ")")
                      (trim ())
                      (trim "in" ())
                      (trim "from" ())
                      (average "(" () ")")
                      (average "of" ())
                      (minimum "(" () ")")
                      (minimum "of" ())
                      (minimum "in" ())
                      (maximum "(" () ")")
                      (maximum "of" ())
                      (maximum "in" ())
                      (count "(" () ")")
                      (count "in" ())
                      (count "of" ())
                      (sum "(" () ")")
                      (sum "of" ())
                      (add-seconds ())
                      (add-hours ())
                      (add-minutes ())
                      (add-days ())
                      (add-months ())
                      (add-years ())
                      (subtract-seconds ())
                      (subtract-hours ())
                      (subtract-minutes ())
                      (subtract-days ())
                      (subtract-months ())
                      (subtract-years ())
                      (extract "seconds" "from" ())
                      (extract "minutes" "from" ())
                      (extract "hours" "from" ())
                      (extract "days" "from" ())
                      (extract "months" "from" ())
                      (extract "years" "from" ())
                      (extract "epoch" "from" ())
                      (extract "date" "from" ())
                      (extract "day" "of" "year" "from" ())
                      (extract "day" "of" "week" "from" ())
                      )))

    ; val-f is applied to every element of the list
    ; pred-f is applied on that transformed value
    (define (find-f pred-f val-f lst)
      (if (null? lst)
          #f
          (let ((val (val-f (car lst))))
            (if (pred-f val)
                val
                (find-f pred-f val-f (cdr lst))))))

    ; wls is word lists that corresponds to a symbol
    ; wl is a specific world list under consideration
    ; tok is is the keyword we are trying to find e.g. '+
    ; toks is the list of input tokens to be matched
    (define (parse-keyword tok xtokens)
;      (printf "parse-keyword tok: ~a, tokens: ~a\n" tok (if (union? xtokens) (union-contents xtokens) xtokens))
      (for/all ([tokens xtokens])
        (let ((word-lists
               (sort (cdr (find-f (lambda (k) (eq? (car k) tok)) identity keywords))
                     (lambda (x y) (> (length x) (length y))))))
          (letrec ((swallow
                    (lambda (wls toks)
                      (let ((next (car toks)))
                        (find-f
                         identity
                         (lambda (wl)
                           (if (equal? (car wl) next)
                               (if (null? (cdr wl))
                                   (cons tok (cdr toks))
                                   (swallow
                                    (remove '()
                                            (map cdr
                                                 (filter (lambda (x) (equal? (car x) next)) wls)))
                                    (cdr toks)))
                               #f))
                         wls)))))
            (or
             (and
              (not (null? tokens))
              (or
               (if (eq? (car tokens) tok) (cons tok (cdr tokens)) #f)
               (swallow word-lists tokens)))
             (cons #f tokens))))))   ; found nothing, tokens is null, so return #f
    
    (define (parse-op ops)
      (lambda (xtokens)
        (for/all ([tokens xtokens])
          (or
           (find-f
            identity
            (lambda (op)
              (if (equal? op (car tokens))
                  (cons op (cdr tokens))
                  (if (symbol? op)
                      (let ((x (parse-keyword op tokens)))
                        (if (eq? (car x) #f) #f x))
                      #f)))
            ops)
           (cons #f tokens)))))

    (define (parse-nullary-expr xtokens)
      (for/all ([tokens xtokens])
        (let ((next (car tokens)))
          (cond ((equal? "(" next)
                 (let ((x (parse-if (cdr tokens))))
                   (if (and (car x) (equal? (cadr x) ")"))
                       (cons (car x) (cddr x))
                       (list #f tokens))))
                ((not (eq? #f (member next column-names)))
                 (cons (list 'in (+ 1 (- (length column-names) (length (member next column-names))))) (cdr tokens)))
                ((and (string? next) (not (member next reserved)))
                 (cons (or (string->number next) next) (cdr tokens)))
                ((number? next)
                 (cons next (cdr tokens)))            
                (#t
                 (cons #f tokens))))))

    (define parse-unary-op (parse-op '(- not)))

    (define (parse-unary-expr xtokens)
      (for/all ([tokens xtokens])
        (let* ([unary-op (parse-unary-op tokens)]
               [val (car unary-op)]
               [toks (cdr unary-op)]
               [next (car toks)])
          (let ((x (parse-nullary-expr toks)))
            (if (eq? (car x) #f)
                (cons #f tokens)
                (if val
                    (cons (list val (car x)) (cdr x))
                    x))))))
    
    (define parse-binary-op
      (parse-op '(+ * remainder - /)))
    
    (define (parse-binary-stuff parse-inner-expr parse-operator)
      (lambda (xtokens)
        (for/all ([tokens xtokens])
          (let* ([unary-expr (parse-inner-expr tokens)]
                 [val (car unary-expr)]
                 [toks (cdr unary-expr)])
            (if val
                (letrec ((swallow-binary
                          (lambda (lhs rest-toks)
                            (if (null? rest-toks)
                                (cons lhs rest-toks)
                                (let* ([binary-op (parse-operator rest-toks)]
                                       [op (car binary-op)]
                                       [xtoks [cdr binary-op]])
                                  (if op
                                      (let* ([rhs-unary-expr (parse-inner-expr xtoks)]
                                             [rhs (car rhs-unary-expr)]
                                             [rtoks (cdr rhs-unary-expr)])
                                        (if rhs
                                            (swallow-binary (list op lhs rhs) rtoks)
                                            (cons lhs rest-toks)))
                                      (cons lhs rest-toks)))))))
                  (swallow-binary val toks))
                (cons #f tokens))))))
    
    (define parse-binary-expr 
      (parse-binary-stuff parse-unary-expr parse-binary-op))

    (define parse-comparison-op (parse-op '(<= >= < > != =)))

    (define parse-comparison-expr
      (parse-binary-stuff parse-binary-expr parse-comparison-op))
      
    (define parse-andor-op (parse-op '(and or)))

    (define parse-andor-expr
      (parse-binary-stuff parse-comparison-expr parse-andor-op))

    (define (parse-if xtokens)
      (for/all ([tokens xtokens])
        (let ((x (parse-keyword 'if tokens)))
          (if (eq? (car x) 'if)
              (let ((test (parse-andor-expr (cdr x))))
                (if (not (eq? #f (car test)))
                    (let ((then-test (parse-keyword 'then (cdr test))))
                      (if (not (eq? #f (car then-test)))
                          (let ((then (parse-if (cdr then-test))))
                            (if (eq? #f (car then))
                                (cons (list 'if (car test)) (cdr test))
				(let ((else-test (parse-keyword 'else (cdr then))))
				  (if (car else-test)
				      (let ((else (parse-if (cdr else-test))))
					(if (car else)
					    (cons (list 'if (car test) (car then) (car else)) (cdr else))
					    (cons (list 'if (car test) (car then)) (cdr then))))
				      (cons (list 'if (car test) (car then)) (cdr then))))))
			  (cons (list 'if (car test)) (cdr test))))
                    (cons (list 'if (car test)) (cdr test))))
              (parse-andor-expr tokens)))))

    (define (parse-templates xtokens)
      (for/all ([tokens xtokens])
        (letrec ((parse-template
                  (lambda (ts)
                    (if (null? ts) (cons #f tokens)
                        (let ((template (car ts)))
                          (letrec ((fail #f)
                                   (rest '())
                                   (parse-term
                                    (lambda (toks terms)
                                      (if (null? terms)
                                          (begin (set! rest toks) '())
                                          (cond [(string? (car terms))
                                                 (if (equal? (car terms) (car toks))
                                                     (cons (car terms) (parse-term (cdr toks) (cdr terms)))
                                                     (set! fail #t))]
                                                [(symbol? (car terms))
                                                 (let ((x (parse-keyword (car terms) toks)))
                                                   (if (eq? #f (car x))
                                                       (set! fail #t)
                                                       (cons (car x) (parse-term (cdr x) (cdr terms)))))]
                                                [#t
                                                 (let ((x (parse-if toks)))
                                                   (if (eq? #f (car x))
                                                       (set! fail #t)
                                                       (cons (car x) (parse-term (cdr x) (cdr terms)))))])))))
                            (let ((v (parse-term tokens template)))
                              (if fail (parse-template (cdr ts)) (cons v rest)))))))))
          (let ((v (parse-template templates)))
            (if (eq? #f (car v))
                (parse-if tokens)
                v)))))
    
    (define (parse-group xtokens)
      (for/all ([tokens xtokens])
        (let* ((x (parse-templates tokens)) ; parse-if returns a pair - a symbol if it finds any (or #f if it doesnt) and the rest of the tokens
                   ; to be processed
               (y (parse-keyword 'group (cdr x))))
          (if (eq? (car y) 'group)
              (let ((e (parse-templates (cdr y))))
                (if (not (eq? (car e) #f))
                    (cons (list 'group (car x) (car e)) (cdr e))
                    x))
              x))))
                    
    
    (define (parse-loop xtokens)
      (for/all ([tokens xtokens])
        (if (null? tokens)
            '()
            (let* ([binary-expr (parse-group tokens)] 
                   [val (car binary-expr)]
                   [toks (cdr binary-expr)])
              (if val
                  (cons val (parse-loop toks))
                  (cons (car tokens) (parse-loop (cdr tokens))))))))

    (define (parsish-loop tokens)
      (let ((toks (map (lambda (w) (if (cons? w) (apply choose* w) w)) tokens)))
        (if (equal? toks tokens)
            (parse-loop tokens)
            (values (parse-loop toks) toks))))
    
    parsish-loop))

(define (find-parse parser tokens f)
  (let-values (([parse toks] (parser tokens)))
    (let ((result (solve (assert (f parse)))))
      (values (evaluate parse result) (evaluate toks result)))))


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
           (result (p '("fetch" "day" "of" "year" "A"))))
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

(provide make-parser find-parse)
