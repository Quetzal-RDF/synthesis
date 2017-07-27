#lang rosette

(require rosette/lib/angelic)

(define (make-parser . column-names)
  (let* ((keywords
          '((or ("or") ("either") ("any") ("instead"))
            (and ("and") ("both") ("also") ("including"))
            (not ("not") ("never") ("neither") ("no") ("don't") ("isn't"))
            (concatenate ("concatenate") ("concat") ("combine") ("transform") ("append") ("merge") ("mix") ("encode"))
            (quotient ("quotient"))
            (remainder ("remainder")("modulo")("mod")("%"))
            (abs ("abs") ("absolute") ("magnitude") ("positive"))
            (ceiling ("ceiling") ("round" "up"))
            (floor ("floor") ("round" "down"))
            (truncate ("truncate")("trunc")("drop") ("shorten") ("abbreviate") ("cut"))
            (sign ("sign") ("positive" "or" "negative"))
            (if ("if") ("whether")("when")("unless")("case")("in case")("case" "when"))
            (then ("then") ("do") ("then" "do") ("subsquently"))
            (else ("else") ("or" "else") ("or") ("otherwise"))
            (substring ("substring") ("extract" "characters") ("extract" "string") ("extract" "substring") ("left") ("right") ("mid"))
            (index-of ("indexof")("index" "of")("find")("search"))
            (length ("length")("string" "length")("len"))
            (+ ("+") ("plus"))
            (- ("-") ("minus") ("takeaway") ("subtract") ("deduct"))
            (/ ("/") ("divide") ("divided by"))
            (exponent ("exponent") ("exp") ("power"))
            (logarithm ("logarithm") ("log") ("ln") ("natural logarithm"))
            (sqrt ("sqrt") ("square" "root") ("square" "root" "of"))
            (lower ("lower") ("to" "lower" "case") ("to" "lower") ("convert" "to" "lower" "case"))
            (upper ("upper") ("to" "upper" "case") ("to" "upper") ("convert" "to" "upper" "case"))
            (trim ("trim") ("remove" "trailing" "spaces") ("pare") ("cut"))
            (replace ("replace") ("overlay") ("change"))
            (matches ("matches") ("contains") ("includes") ("has") ("appears in"))
            (now ("now") ("current" "time") ("today"))
            (extract ("extract") ("retrieve") ("pull" "out") ("grab") ("fetch") ("find") ("obtain") ("get"))
            (> (">") ("greater") ("greater" "than") ("more") ("more" "than") ("larger") ("larger" "than") ("higher") ("higher" "than") ("bigger") ("bigger" "than") ("older") ("older" "than"))
            (< ("<") ("less") ("less" "than") ("smaller") ("smaller" "than") ("lower") ("lower" "than") ("younger") ("younger" "than"))
            (>= (">=") ("greater" "than" "or" "equal" "to") ("more" "than" "or" "equal" "to") ("larger" "than" "or" "equal" "to") ("higher" "than" "or" "equal" "to") ("bigger" "than" "or" "equal" "to") ("older" "than" "or" "equal" "to"))
            (<= ("<=") ("less" "than" "or" "equal" "to") ("smaller" "than" "or" "equal" "to") ("lower" "than" "or" "equal" "to") ("younger" "than" "or" "equal" "to"))
            (= ("=") ("==") ("equals") ("is") ("is equal to"))))
         (reserved (filter string? (flatten keywords))))
    
    (define (find-f pred-f val-f lst)
      (if (null? lst)
          #f
          (let ((val (val-f (car lst))))
            (if (pred-f val)
                val
                (find-f pred-f val-f (cdr lst))))))

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
             (if (eq? (car tokens) tok) (cons tok (cdr tokens)) #f)
             (swallow word-lists tokens)
             (cons #f tokens))))))
    
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
                 (cons (list 'column (+ 1 (- (length column-names) (length (member next column-names))))) (cdr tokens)))
                ((or (and (string? next) (not (member next reserved))) (number? next))
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
      (parse-op '(+ "*" "times" '- '/)))
    
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

    (define parse-comparison-op (parse-op '(= < > <= >=)))

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
    
    (define (parse-loop xtokens)
      (for/all ([tokens xtokens])
        (if (null? tokens)
            '()
            (let* ([binary-expr (parse-if tokens)]
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

(provide make-parser find-parse)
