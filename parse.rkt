#lang rosette

(require rosette/lib/angelic)

(define-syntax nullary-function-forms
  (syntax-rules ()
    ((_ symbol ...)
     (append
      (list
       (list '(symbol) (lambda (x) x))
       (list '(symbol "(" ")") (lambda (x) (list (quote symbol)))))
      ...))))

(define-syntax unary-function-forms
  (syntax-rules ()
    ((_ symbol ...)
     (append
      (list
       (list '(symbol "of" ()) (lambda (x) (list (quote symbol) (list-ref x 2))))
       (list '(symbol "in" ()) (lambda (x) (list (quote symbol) (list-ref x 2))))
       (list '(symbol "from" ()) (lambda (x) (list (quote symbol) (list-ref x 2))))
       (list '(symbol "value" ()) (lambda (x) (list (quote symbol) (list-ref x 2))))
       (list '(symbol "value" "of" ()) (lambda (x) (list (quote symbol) (list-ref x 3))))
       (list '(symbol "(" () ")") (lambda (x) (list (quote symbol) (list-ref x 2))))
       (list '(symbol ()) (lambda (x) (list (quote symbol) (list-ref x 1)))))

      ...))))

(define-syntax binary-function-forms
  (syntax-rules ()
    ((_ symbol ...)
     (append
      (list
       (list '(symbol () "and" ()) (lambda (x) (list (quote symbol) (list-ref x 1) (list-ref x 3))))
       (list '(symbol "of" () "in" ()) (lambda (x) (list (quote symbol) (list-ref x 2) (list-ref x 4))))
       (list '(symbol () ()) (lambda (x) (list (quote symbol) (list-ref x 1) (list-ref x 2))))
       (list '(symbol "(" () "," () ")") (lambda (x) (list (quote symbol) (list-ref x 2) (list-ref x 4)))))
      ...))))

(define-syntax ternary-function-forms
  (syntax-rules ()
    ((_ symbol ...)
     (append
      (list
       (list '(symbol "(" () "," () "," () ")") (lambda (x) (list (quote symbol) (list-ref x 2) (list-ref x 4) (list-ref x 6))))
       (list '(symbol () () ()) (lambda (x) (list (quote symbol) (list-ref x 1) (list-ref x 2) (list-ref x 3))))
       (list '(symbol () "and" () "and" ()) (lambda (x) (list (quote symbol) (list-ref x 1) (list-ref x 3) (list-ref x 5)))))
      ...))))

(define-syntax grouping-function-forms
  (syntax-rules ()
    ((_ (symbol group func) ...)
     (append
      (list
       (list '(symbol () group ()) (lambda (x) (list (quote func) (list-ref x 1) (list-ref x 3))))
       (list '(symbol "(" () ")" group ()) (lambda (x) (list (quote func) (list-ref x 2)(list-ref x 5))))
       (list '(symbol "of" () group ()) (lambda (x) (list (quote func) (list-ref x 2)(list-ref x 4))))
       (list '(symbol "(" () ")" group ()) (lambda (x) (list (quote func) (list-ref x 2)(list-ref x 5)))))
      ...))))

(define (make-parser . xcolumn-names)
  (let* ((column-names
          (map (lambda (s) (string-trim s #px"\\$.+")) xcolumn-names))
         (keywords
          '((or ("or") ("either") ("any") ("instead"))
            (null ("null") ("empty")("blank"))
            (is-null ("is" "null") ("is" "empty"))
            (is-not-null ("is" "not" "null") ("is" "not" "empty"))
            (and ("and") ("both") ("also") ("including"))
            (not ("not") ("never") ("neither") ("no") ("don't") ("isn't")("is" "not"))
            (concat ("concatenate") ("concat") ("combine") ("transform") ("append") ("merge") ("mix") ("encode"))
            (quotient ("quotient"))
            (group-concat ("aggregate" "strings") ("concatenate" "strings")("aggregate" "all" "strings") ("concatenate" "all" "strings"))
            (remainder ("remainder")("remainder")("modulo")("mod")("%"))
            (abs ("abs") ("absolute") ("magnitude") ("positive"))
            (ceiling ("ceiling") ("round" "up"))
            (floor ("floor") ("round" "down"))
            (truncate ("truncate")("trunc")("drop") ("shorten") ("abbreviate") ("cut"))
            (sign ("positive" "or" "negative" "sign")("positive" "or" "negative")("sign"))
            (if ("if") ("whether")("when")("unless")("case")("in case")("case" "when"))
            (then ("then") ("do" "subsequently") ("do") ("then" "do") ("subsquently"))
            (coalesce ("coalesce")("if" "null")("is" "null")("nvl"))
            (else ("else") ("or" "else") ("or") ("otherwise"))
            (substring ("substring")("left") ("right") ("mid"))
            (substring-first ("extract" "characters") ("extract" "string") ("extract" "substring")("find")("find" "characters")("find" "substring"))
            (substring-start ("starting" "at") ("start" "at"))
            (substring-end ("ending" "at") ("end" "at"))
            (index-of ("index" "of")("position" "of")("find" "index")("find" "index" "of")("find" "position")("find" "position" "of")("search" "position")("search" "position" "of"))
            (length ("length")("string" "length")("len")("length" "of")("string" "length" "of")("character" "length" "of")("number" "of" "characters" "in"))
            (in-list ("values" "in")("is" "one" "of")("member"))
            (+ ("+") ("plus"))
            (- ("-") ("minus") ("takeaway") ("subtract") ("deduct"))
            (/ ("/") ("divide") ("divided" "by"))
            (* ("*") ("multiply" "by") ("multiplied" "by") ("multiply") ("times"))
            (exponent ("exponent") ("exp") ("power"))
            (logarithm ("logarithm") ("logarithm" "base" "10"))
            (natural-logarithm ("ln") ("natural" "logarithm"))
            (sqrt ("sqrt") ("square" "root") ("square" "root" "of"))
            (lower ("to" "lower" "case")("lower"))
            (upper ("to" "upper" "case") ("upper"))
            (trim ("trim") ("remove" "spaces") ("remove" "extra" "spaces")("remove" "extra" "white" "spaces")("remove" "white" "spaces") ("pare") ("pare" "spaces")("pare" "extra" "spaces")("pare" "white" "spaces")("pare" "extra" "white" "spaces")("cut" "spaces")("cut" "white" "spaces")("cut" "extra" "white" "spaces"))
            (replace ("replace") ("overlay") ("change"))
            (like ("like") ("matches")("is" "like"))
            (now ("now") ("current" "time") ("time" "now") ("today"))
            (extract ("extract") ("retrieve") ("pull" "out") ("grab") ("fetch") ("find") ("obtain") ("get"))
            (> (">") ("after") ("is" "after") ("greater") ("is" "greater" "than") ("greater" "than") ("more") ("more" "than") ("is" "more" "than") ("larger") ("larger" "than") ("is" "larger" "than") ("higher") ("higher" "than") ("is" "higher" "than") ("bigger") ("bigger" "than") ("is" "bigger" "than") ("older") ("older" "than") ("is" "older" "than"))
            (< ("<") ("before") ("is" "before") ("less") ("less" "than") ("is" "less" "than") ("smaller") ("smaller" "than") ("is" "smaller" "than") ("lower") ("lower" "than") ("is" "lower" "than") ("younger") ("younger" "than") ("is" "younger" "than"))
            (>= (">=") ("greater" "than" "or" "equal" "to") ("is" "greater" "than" "or" "equal" "to") ("more" "than" "or" "equal" "to") ("is" "more" "than" "or" "equal" "to") ("larger" "than" "or" "equal" "to") ("is" "larger" "than" "or" "equal" "to") ("higher" "than" "or" "equal" "to") ("is" "higher" "than" "or" "equal" "to") ("bigger" "than" "or" "equal" "to") ("is" "higher" "than" "or" "equal" "to") ("older" "than" "or" "equal" "to") ("is" "older" "than" "or" "equal" "to"))
            (<= ("<=") ("less" "than" "or" "equal" "to") ("is" "less" "than" "or" "equal" "to") ("smaller" "than" "or" "equal" "to") ("is" "smaller" "than" "or" "equal" "to") ("lower" "than" "or" "equal" "to") ("is" "lower" "than" "or" "equal" "to") ("is" "younger" "than" "or" "equal" "to"))
            (!= ("!=") ("<>") ("not equal") ("is" "not" "equal" "to") ("is" "not" "same" "as"))
            (= ("=") ("==") ("equals") ("is") ("is" "equal" "to") ("is" "same" "as"))
            (avg ("average") ("mean") ("avg"))
            (min ("minimum") ("min") ("lowest") ("lowest" "value"))
            (max ("maximum") ("max") ("highest") ("highest" "value"))
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
            (round ("round")("round" "up")("round" "off"))
            (between ("between"))
            (add ("add"))
            (subtract ("subtract"))
            (group ("group") ("group" "by") ("grouped" "by") ("organize" "by") ("organized" "by") ("cluster" "by") ("clustered" "by") ("by") ("based" "on"))))
         (reserved (filter string? (flatten keywords)))
         (templates (append
                     (grouping-function-forms (avg group average-group) (sum group sum-group) (count group count-group) (max group maximum-group) (min group minimum-group))
                     (ternary-function-forms between replace substring)
                     (binary-function-forms index-of concat like exponent quotient remainder add-seconds add-minutes add-hours add-days add-months
                                           add-years subtract-seconds subtract-hours subtract-minutes subtract-days subtract-months subtract-years in-list coalesce)
                     (unary-function-forms group-concat is-null is-not-null abs round ceiling floor truncate sign logarithm natural-logarithm
                                           sqrt upper lower length trim avg min max count sum not) 
                     (nullary-function-forms now)
                     (list
                      (list '(index-of () "in" ()) (lambda (x) (list 'index-of (list-ref x 1) (list-ref x 3))))
                      (list '(substring-first "from" () substring-start () substring-end ()) (lambda (x) (list 'substring (list-ref x 2) (list-ref x 4) (list-ref x 6))))
                      (list '(concat () concatenate-with ()) (lambda (x) (list 'concat (list-ref x 1) (list-ref x 3))))
                      (list '(add () "seconds" "to" ()) (lambda (x) (list 'add-seconds (list-ref x 1)(list-ref x 4))))
                      (list '(add () "minutes" "to" ()) (lambda (x) (list 'add-minutes (list-ref x 1)(list-ref x 4))))
                      (list '(add () "hours" "to" ()) (lambda (x) (list 'add-hours (list-ref x 1)(list-ref x 4))))
                      (list '(add () "days" "to" ()) (lambda (x) (list 'add-days (list-ref x 1)(list-ref x 4))))
                      (list '(add () "months" "to" ()) (lambda (x) (list 'add-months (list-ref x 1)(list-ref x 4))))
                      (list '(add () "years" "to" ()) (lambda (x) (list 'add-years (list-ref x 1)(list-ref x 4))))
                      
                      (list '(subtract () "seconds" "from" ()) (lambda (x) (list 'subtract-seconds (list-ref x 1)(list-ref x 4))))
                      (list '(subtract () "minutes" "from" ()) (lambda (x) (list 'subtract-minutes (list-ref x 1)(list-ref x 4))))
                      (list '(subtract () "hours" "from" ()) (lambda (x) (list 'subtract-hours (list-ref x 1)(list-ref x 4))))
                      (list '(subtract () "days" "from" ()) (lambda (x) (list 'subtract-days (list-ref x 1)(list-ref x 4))))
                      (list '(subtract () "months" "from" ()) (lambda (x) (list 'subtract-months (list-ref x 1)(list-ref x 4))))
                      (list '(subtract () "years" "from" ()) (lambda (x) (list 'subtract-years (list-ref x 1)(list-ref x 4))))

                      (list '(extract "seconds" "from" ()) (lambda (x) (list 'extract-seconds (list-ref x 3))))
                      (list '(extract "minutes" "from" ()) (lambda (x) (list 'extract-minutes (list-ref x 3))))
                      (list '(extract "hours" "from" ()) (lambda (x) (list 'extract-hours (list-ref x 3))))
                      (list '(extract "days" "from" ()) (lambda (x) (list 'extract-days (list-ref x 3))))
                      (list '(extract "months" "from" ()) (lambda (x) (list 'extract-months (list-ref x 3))))
                      (list '(extract "years" "from" ()) (lambda (x) (list 'extract-years (list-ref x 3))))
                      (list '(extract "epoch" "from" ()) (lambda (x) (list 'date-to-epoch (list-ref x 3))))
                      (list '(extract "date" "from" ()) (lambda (x) (list 'date-from-epoch (list-ref x 3))))
                      (list '(extract "day" "of" "year" "from" ()) (lambda (x) (list 'extract-day-of-year (list-ref x 5))))
                      (list '(extract "day" "of" "week" "from" ()) (lambda (x) (list 'extract-day-of-week (list-ref x 5))))
   ;                   (list '(() "is" null) (lambda (x) (list 'is-null (list-ref x 0))))
                                
                      ))))

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
                ((string->number next)
                 (cons (string->number next) (cdr tokens)))            
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

    (define (parse-templates xtokens)
      (for/all ([tokens xtokens])
        (letrec ((parse-template
                  (lambda (ts)
                    (if (null? ts) (cons #f tokens)
                        (let ((template (caar ts))
                              (func (cadr (car ts))))
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
                                                 (let ((x (parse-comparison-expr toks)))
                                                   (if (eq? #f (car x))
                                                       (set! fail #t)
                                                       (cons (car x) (parse-term (cdr x) (cdr terms)))))])))))
                            (let ((v (parse-term tokens template)))
                              (if fail (parse-template (cdr ts)) (cons (func v) rest)))))))))
          (let ((v (parse-template templates)))
            (if (eq? #f (car v))
                (parse-comparison-expr tokens)
                v)))))
      
    (define parse-andor-op (parse-op '(and or)))

    (define parse-andor-expr
      (parse-binary-stuff parse-templates parse-andor-op))

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
