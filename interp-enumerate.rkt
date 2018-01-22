#lang rosette

(error-print-width 1000)

(current-bitwidth #f)
(require math/base)
(require math/statistics)
(require racket/async-channel)
(require rosette/solver/smt/z3)
(require srfi/19)

(require "../rosette/rosette/solver/smt/server.rkt")
(require "dates.rkt")

(require "threads.rkt")

(define vals (make-hash))

(define (sign b)
  (cond
    [(positive? b) 1]
    [(zero? b) 0]
    [(negative? b) -1]
))

(define/match (var? e)
  [((expression op child ...)) #f]
  [((? constant?)) #t]
  [(_) #f])

(define (id pos)
  (string->symbol (format "~S" pos)))

(define (clear-vals!)
  (hash-clear! vals))

(define (val pos type)
  (let ((name (id pos)))
    (if (hash-has-key? vals (cons name type))
        (hash-ref vals (cons name type))
        (let ((v (constant name type)))
          (hash-set! vals (cons name type) v)
          v))))

(define (merge . args)
  (apply append (filter cons? args)))

(define logging-processor%
  (class object%
    (super-new)

    (define/public (constant v)
      '())

    (define/public (basic-unary f v)
      (cons f v))
    
    (define/public (basic-binary f l r)
      (cons f (merge l r)))
    
    (define/public (is-null-v? mb v pos)
      (cons 'is-null v))

    (define/public (is-null? pos)
      (list do-is-null?))
    
    (define/public (in pos type-f)
      (if (eq? type-f number?)
          (list do-in-int)
          (list do-in-str)))
      	    
    (define/public (in-v pos v type-f)
      (if (eq? type-f number?)
          (list do-in-int)
          (list do-in-str)))

    (define/public (symbolic pos type)
      (list
       (if (equal? type integer?)
	   do-intv
	   do-strv)))
    
    (define/public (basic-math pos l r)
      (cons do-basic-math (merge l r)))

    (define/public (basic-num-functions pos v)
      (merge (list do-basic-num-functions) v))
    
    (define/public (index-of pos l r)
      (cons do-index-of (merge l r)))
    
    (define/public (compare-to pos l r)
      (cons do-compare-to (merge l r)))

    (define/public (compare-to-str pos l r)
      (cons do-compare-to-str (merge l r)))
    
    (define/public (logic-op pos l r)
      (cons do-logic-op (merge l r)))

    (define/public (logic-op-not pos v)
      (merge (list do-logic-op-not) v))

    (define/public (if-then-else case l r)
      (cons (if (number? l) do-if-then-int do-if-then-str) (merge case l r)))
    
    (define/public (strlength pos str)
      (merge (list do-length) str))
    
    (define/public (substr str l r)
      (cons do-substring (merge str l r)))

    (define/public (concat pos left right)
      (cons do-concat (merge left right)))

    (define/public (get-digits pos str)
      (merge (list do-get-digits) str))

    (define/public (trim pos str)
      (merge (list do-trim) str))

    (define/public (date-diff pos left right)
      (merge (list do-date-diff) left right))

    (define/public (date-compare pos left right)
      (merge (list do-date-compare) left right))
    
    (define/public (date-interval pos left right)
       (merge (list do-date-interval) left right))

    (define/public (date-extract pos v)
       (merge (list do-date-extract) v))

    (define/public (date-to-epoch pos v)
      (merge (list do-date-to-epoch) v))

    (define/public (date-from-epoch pos v)
      (merge (list do-date-from-epoch) v))

    (define/public (like pos lhs rhs)
      (merge (list do-like) lhs rhs))

    (define/public (set-seconds-to l r)
      (merge (list 'set-seconds-to) l r))
    
    (define/public (set-minutes-to l r)
      (merge (list 'set-minutes-to) l r))
    
    (define/public (set-hours-to l r)
      (merge (list 'set-hours-to) l r))

    (define/public (set-days-to l r)
      (merge (list 'set-days-to) l r))

    (define/public (set-months-to l r)
      (merge (list 'set-months-to) l r))

    (define/public (set-years-to l r)
      (merge (list 'set-years-to) l r))

    (define/public (set-to-first-day-of-month v)
      (merge (list 'set-to-first-day-of-month) v))

    (define/public (set-to-last-day-of-month v)
      (merge (list 'set-to-last-day-of-month) v))

    (define/public (set-to-first-month v)
      (merge (list 'set-to-first-month) v))

    (define/public (set-to-last-month v)
      (merge (list 'set-to-last-month) v))

    (define/public (set-to-next-day v)
      (merge (list 'set-to-next-day) v))

    (define/public (set-to-previous-day v)
      (merge (list 'set-to-previous-day) v))

    (define/public (set-to-next-month v)
      (merge (list 'set-to-next-month) v))

    (define/public (set-to-previous-month v)
      (merge (list 'set-to-previous-month) v))

    (define/public (is-in-current-month v)
      (merge (list 'is-in-current-month) v))

    (define/public (is-in-last-x-months l r)
      (merge (list 'is-in-last-x-months) l r))

    (define/public (general-compare date-op number-op left right)
      (merge (list 'general-compare) number-op left right))

    (define/public (aggregate-op pos type v op is-average)
      (merge (list 'agg) op v))
  
    (define/public (aggregate pos type v)
      (let* ((op
              (if (eq? type 'string)
                  'group-concat
                  (let ((v1 (val (cons 1 pos) boolean?))
                        (v2 (val (cons 2 pos) boolean?)))
                    (cond ((and v1 v2) 'sum)
                          ((and v1 (not v2)) 'max)
                          (#t 'min))))))
        (merge (list op) v)))))

(define doc-processor%
  (class object%
    (super-new)

    (define/public (constant v)
      v)

    (define/public (basic-unary f v)
      (list f v))
    
    (define/public (basic-binary f l r)
      (list (if (equal? f equal?) '== (object-name f)) l r))
    
    (define/public (is-null-v? mb v pos)
        (list (if mb 'is-null 'is-not-null) v))

    (define/public (is-null? pos)
      (let ((mb (val (cons 'is-null pos) boolean?))
            (mi (val (cons 'argn pos) integer?)))
        (list (if mb 'is-null 'is-not-null) mi)))
      
    (define/public (in pos type-f)
      (let ((m (val (cons 'argn pos) integer?)))
        (list 'in m)))

    (define/public (in-v pos v type-f)
      (list 'in v))

    (define/public (symbolic pos type)
      (val pos type))
    
    (define/public (basic-math pos l r)
      (let ((m1 (val (cons 'm1 pos) boolean?))
            (m2 (val (cons 'm2 pos) boolean?))
            (m3 (val (cons 'm3 pos) boolean?)))
        (list
         (cond
           [(and m1 m2 m3) quotient] 
           [(and m1 m2 (not m3)) remainder]
           [(and m1 (not m2) m3) /]
           [(and m1 (not m2) (not m3)) *]
           [(and (not m1) m2 m3) -]
           [#t +])
         l r)))

    (define/public (basic-num-functions pos v)
      (let ((m1 (val (cons 'm1 pos) boolean?))
            (m2 (val (cons 'm2 pos) boolean?))
            (m3 (val (cons 'm3 pos) boolean?)))
            (list
             (if m1 (if m2 (if m3 'abs 'truncate) (if m3 'sign 'ceiling)) (if m2 'floor 'round))
             v)))
    
    (define/public (index-of pos l r)
      (list 'index-of l r))
    
    (define/public (compare-to pos l r)
      (let ((islt (val (cons 'islt pos) boolean?))
            (iseq (val (cons 'iseq pos) boolean?)))
        (list (if iseq (if islt '<= '>=) (if islt '< '>)) l r)))

    (define/public (compare-to-str pos l r)
        (list '== l r))
    
    (define/public (logic-op pos l r)
      (let ((isand (val (cons 'isand pos) boolean?)))
        (list (if isand 'and 'or) l r)))

    (define/public (logic-op-not pos v)
        (list 'not v))

    (define/public (if-then-else case l r)
      (list 'if case l r))
    
    (define/public (strlength pos str)
      (list 'length str))
    
    (define/public (substr str l r)
      (list 'substring str l r))

    (define/public (concat pos left right)
      (list 'concat left right))

    (define/public (date-diff pos left right)
      (list 'date-subtract left right))

  
    (define/public (date-compare pos left right)
      (let ((di1 (val (cons 'di1 pos) boolean?))
            (di2 (val (cons 'di2 pos) boolean?))
            (di3 (val (cons 'di3 pos) boolean?)))
            (list
             (if di1
                 (if di2
                     (if di3 'date-le 'date-ge)
                     (if di3 'date-gt 'date-lt))
                 (when di2 'date-equal))
             left right)))
    
    (define/public (date-interval pos left right)
      (let ((di1 (val (cons 'di1 pos) boolean?))
            (di2 (val (cons 'di2 pos) boolean?))
            (di3 (val (cons 'di3 pos) boolean?))
            (di4 (val (cons 'di4 pos) boolean?)))
            (list
             (if di1
                 (if di2
                     (if di3
                         (if di4 'add-seconds 'add-minutes)
                         (if di4 'add-hours 'add-days))
                     (if di3
                         (if di4 'add-months 'add-years)
                         (if di4 'subtract-seconds 'subtract-minutes)))
                 (if di2
                     (if di3 subtract-hours subtract-days)
                     (if di3 subtract-months subtract-years)))
             left right)))

    (define/public (date-extract pos v)
          (let ((de1 (val (cons 'de1 pos) boolean?))
                (de2 (val (cons 'de2 pos) boolean?))
                (de3 (val (cons 'de3 pos) boolean?)))
            (list
                  (if de1
                      (if de2
                          (if de3 'extract-seconds 'extract-minutes)
                          (if de3 'extract-hours 'extract-days))
                      (if de2
                          (if de3 'extract-months 'extract-years)
                          (if de3 'extract-day-of-year 'extract-day-of-week))) v)))

    (define/public (date-to-epoch pos v)
      (list 'date-to-epoch v))

    (define/public (set-seconds-to l r)
      (list 'set-seconds-to l r))
    
    (define/public (set-minutes-to l r)
      (list 'set-minutes-to l r))
    
    (define/public (set-hours-to l r)
      (list 'set-hours-to l r))

    (define/public (set-days-to l r)
      (list 'set-days-to l r))

    (define/public (set-months-to l r)
      (list 'set-months-to l r))

    (define/public (set-years-to l r)
      (list 'set-years-to l r))

    (define/public (set-to-first-day-of-month v)
      (list 'set-to-first-day-of-month v))

    (define/public (set-to-last-day-of-month v)
      (list 'set-to-last-day-of-month v))

    (define/public (set-to-first-month v)
      (list 'set-to-first-month v))

    (define/public (set-to-last-month v)
      (list 'set-to-last-month v))

    (define/public (set-to-next-day v)
      (list 'set-to-next-day v))

    (define/public (general-compare date-op number-op left right)
      (if (equal? number-op equal?)
          (list '= left right) (list number-op left right)))

    (define/public (set-to-previous-day v)
      (list 'set-to-previous-day v))

    (define/public (set-to-next-month v)
      (list 'set-to-next-month v))

    (define/public (set-to-previous-month v)
      (list 'set-to-previous-month v))

    (define/public (is-in-current-month v)
      (list 'is-in-current-month v))

    (define/public (is-in-last-x-months l r)
     (list 'is-in-last-x-months l r))

    (define/public (date-from-epoch pos v)
      (list 'date-from-epoch v))

    (define/public (get-digits pos str)
      (list 'get-digits str))

    (define/public (trim pos str)
      (list 'trim str))

    (define/public (aggregate-op pos type v op is-average)
      (list 'agg op v))
    
    (define/public (like pos lhs rhs)
      (let ((m1 (val (cons 'li1 pos) boolean?))
                (m2 (val (cons 'li2 pos) boolean?)))
        (list 'like lhs
        (if m1
            (if m2 (string-append rhs "%") (string-append "%" rhs))
            (if m2 (string-append "%" rhs "%") rhs)))))

    (define/public (aggregate pos type v)
      (let ((op
             (if (eq? type 'string)
                 'group-concat
                 (let ((v1 (val (cons 1 pos) boolean?))
                       (v2 (val (cons 2 pos) boolean?)))
                   (cond ((and v1 v2) 'min)
                         ((and v1 (not v2)) 'max)
                         ((and (not v1) (not v2)) 'avg)
                         (#t 'sum))))))
        (list 'agg op v)))))

(define (basic-math-op r pos + - * / quotient remainder)
  (let ((m1 (val (cons 'm1 pos) boolean?))
        (m2 (val (cons 'm2 pos) boolean?))
        (m3 (val (cons 'm3 pos) boolean?)))
    (cond [(and m1 m2 m3) (if (and (integer? r) (not (= r 0))) quotient 'invalid)]
          [(and m1 m2 (not m3)) (if (and (integer? r) (not (= r 0))) remainder 'invalid)]
          [(and m1 (not m2) m3) (if (not (= r 0)) / 'invalid)]
          [(and m1 (not m2) (not m3)) *]
          [(and (not m1) m2 m3) -]
          [#t +])))

(define (lifted-round v)
  (truncate (+ v .5)))

(define percent-regex (regexp (string-append "[" (~a (integer->char 33)) "-" (~a (integer->char 126)) "]*")))

(define underscore-regex (regexp (string-append "[" (~a (integer->char 33)) "-" (~a (integer->char 126)) "]")))

(define (create-like-list indexes pat)
  (let ((result (for/list ([x indexes]
                          [y (in-range (length indexes))])
                 (let ((z (if (equal? (string-ref pat x) #\%)
                              percent-regex
                              underscore-regex)))
                   (cond
                     [(and (> x 0) (= y 0)) (list (regexp-quote (substring pat 0 x)) z)]
                     [(and (> x 0) (> y 0)) (if (> (- x (list-ref indexes (- y 1))) 1)
                                                (list (regexp-quote (substring pat (+ (list-ref indexes (- y 1)) 1) x)) z)
                                                (list z))]
                     [#t (list z)])))))
    (if (< (last indexes) (- (string-length pat) 1))
      (flatten (append result (list (regexp-quote (substring pat (+ (last indexes) 1) (string-length pat))))))
      (flatten result))))

(define (create-like-constraints lhs like-list)
   (regexp-match-exact? (apply regexp-concat like-list) lhs))

(define (convert-string-to-date str)
  (with-handlers ([exn:fail?
                   (lambda (e) #f)])
    (let ((d (string->date str "~m-~d-~Y")))
      (vector (date-second d) (date-minute d) (date-hour d) (date-day d) (date-month d) (date-year d)))))

(define (dateable? v)
  (or (vector? v)
      (and
       (string? v)
       (not (symbolic? v))(convert-string-to-date v))))

(define-syntax date-case-op
  (syntax-rules ()
    ((_ (arg ...) (body ...) (otherwise ...))
     (if (and (dateable? arg) ...)
         (let ((arg (if (vector? arg) arg (convert-string-to-date arg))) ...)
            body ...)
	 (begin
	   otherwise ...)))))

(define-syntax date-op
  (syntax-rules ()
    ((_ (arg ...) body ...)
     (date-case-op (arg ...) (body ...) ('invalid)))))

(define (like-constant lhs rhs)
  (println "called like constant")
  (println lhs)
  (println rhs)
  (println (string? lhs))
  (println (string? rhs))
  (if (and (string? lhs) (string? rhs))
      (let* ((indexes
              (filter (lambda (x) (not (equal? x -1)))
                      (for/list ([i rhs]
                                 [k (in-range (string-length rhs))])
                         (if (or (equal? i #\%) (equal? i #\_)) k -1))))
             (ll (create-like-list indexes rhs))
             (lc (create-like-constraints lhs ll)))
        (println "like list")
        (println ll)
        (println "constraint")
        (println lc)
        lc)
      'invalid))

(define expr-processor%
  (class object%
    (init inputs)

    (super-new)
    
    (define input-vals inputs)
    
    (define/public (constant v)
      (let ((r
             (if (not (vector? v))
                 v
                 (let ((s (extract-seconds v))
                       (m (extract-minutes v))
                       (h (extract-hours v))
                       (dy (extract-days v))
                       (mn (extract-months v))
                       (yr (extract-years v)))
                   (if (and (>= s 0) (< s 60)
                            (>= m 0) (< m 60)
                            (>= h 0) (< h 24)
                            (> dy 0) (<= dy 31)
                            (> mn 0) (<= mn 12)
                            (> yr 0))
                       v
                       'invalid)))))
        ;(println "value")
        ;(println r)
        r))
    
    (define/public (basic-unary f v)
      (f v))
    
    (define/public (basic-binary f l r)
      ;(println (type-of l))
      ;(println (type-of r))
      (if (or (eq? l 'invalid) (eq? r 'invalid))
          'invalid
          (f l r)))

    (define/public (set-seconds-to l r)
      (create-date r (get-field l "minutes") (get-field l "hours") (get-field l "days") (get-field l "months") (get-field l "years")))
    
    (define/public (set-minutes-to l r)
      (create-date (get-field l "seconds") r (get-field l "hours") (get-field l "days") (get-field l "months") (get-field l "years")))
    
    (define/public (set-hours-to l r)
      (create-date (get-field l "seconds") (get-field l "minutes") r (get-field l "days") (get-field l "months") (get-field l "years")))

    (define/public (set-days-to l r)
      (create-date (get-field l "seconds") (get-field l "minutes") (get-field l "hours") r (get-field l "months") (get-field l "years")))

    (define/public (set-months-to l r)
      (create-date (get-field l "seconds") (get-field l "minutes") (get-field l "hours") (get-field l "days") r (get-field l "years")))

    (define/public (set-years-to l r)
      (create-date (get-field l "seconds") (get-field l "minutes") (get-field l "hours") (get-field l "days") (get-field l "months") r))

    (define/public (set-to-first-day-of-month v)
      (create-date (get-field v "seconds") (get-field v "minutes") (get-field v "hours") 1 (get-field v "months") (get-field v "years")))

    (define/public (set-to-last-day-of-month v)
      (create-date (get-field v "seconds") (get-field v "minutes") (get-field v "hours") (num-days-in-month (get-field v "months")) (get-field v "years")))

    (define/public (set-to-first-month v)
      (create-date (get-field v "seconds") (get-field v "minutes") (get-field v "hours") (get-field v "days") 1 (get-field v "years")))

    (define/public (set-to-last-month v)
      (create-date (get-field v "seconds") (get-field v "minutes") (get-field v "hours") (get-field v "days") 12 (get-field v "years")))

    (define/public (set-to-next-day v)
      (add-days v 1))

    (define/public (set-to-previous-day v)
      (subtract-days v 1))

    (define/public (set-to-next-month v)
      (add-months v 1))

    (define/public (set-to-previous-month v)
      (subtract-months v 1))

    (define/public (is-in-current-month v)
      (equal? (extract-months (extract-date-from-epoch (current-seconds))) (extract-months v)))

    (define/public (is-in-last-x-months l r)
      (>= l (subtract-months (extract-date-from-epoch (current-seconds)) r)))
  
    (define/public (is-null-v? mb v pos)
      (send this basic-unary (lambda (v) (if mb (equal? v '()) (not (equal? v '())))) v))

    (define/public (is-null? pos)
      (let ((mb (val (cons 'is-null pos) boolean?))
            (mi (val (cons 'argn pos) integer?)))
        (if (and (>= mi 1) (< (- mi 1) (length input-vals)))
            (let ((v (list-ref input-vals (- mi 1))))
              (send this is-null-v? mb v pos))
            'invalid)))

    (define/public (in pos type-f)
      (let ((m (val (cons 'argn pos) integer?)))
        (if (and (>= m 1) (< (- m 1) (length input-vals)))
            (send this in-v pos m type-f)
            'invalid)))

    (define/public (in-v pos v type-f)
      (let ((val (list-ref input-vals (- v 1))))
        (if (type-f val)
            (send this constant val)
            'invalid)))
    
    (define/public (symbolic pos type)
      (val pos type))

    (define/public (basic-math pos l r)
      (if (and (number? l) (number? r))
          (let ((op (basic-math-op r pos + - * / quotient remainder)))
            (if (eq? op 'invalid)
                'invalid
                (send this basic-binary op l r)))
          'invalid))

    (define/public (basic-num-functions pos v)
      (if (number? v)
          (let ((m1 (val (cons 'm1 pos) boolean?))
                (m2 (val (cons 'm2 pos) boolean?))
                (m3 (val (cons 'm3 pos) boolean?)))
            (send this basic-unary (if m1 (if m2 (if m3 abs truncate) (if m3 sign ceiling)) (if m2 floor lifted-round)) v))
          'invalid))
    
    (define/public (index-of pos l r)
      (if (and (string? l) (string? r) (<= (string-length r) (string-length l)))
          (string-index-of l r)
          'invalid))

    (define/public (compare-to pos l r)
      (if (and (number? l) (number? r))
          (let ((islt (val (cons 'islt pos) boolean?))
                (iseq (val (cons 'iseq pos) boolean?)))
            (send this basic-binary (if iseq (if islt <= >=) (if islt < >)) l r))
          'invalid))

    (define/public (compare-to-str pos l r)
      (send this basic-binary equal? l r))
    
    (define/public (logic-op-not pos v)
      (if (boolean? v)
          (send this basic-unary not v)
          'invalid))
  
    (define/public (logic-op pos l r)
      (if (and (boolean? l) (boolean? r))
          (let ((isand (val (cons 'isand pos) boolean?)))
            (send this basic-binary (lambda (l r) (if isand (and l r) (or l r))) l r))
           'invalid))
    
    (define/public (if-then-else case l r)
;      (println "IF_THEN_ELSE")
;      (println case)
;      (println l)
;      (println r)
;      (println "********")
 
      (if (and (boolean? case))
          (if case l r)
           'invalid))

    (define/public (strlength pos str)
      (if (string? str)
          (send this basic-unary string-length str)
          'invalid))

    (define/public (substr str l r)
      ;(println "types")
      ;(println str)
      ;(println (type-of str))
      ;(println (type-of l))
      ;(println (type-of r))
      (let ((v (if (and (string? str) (integer? l) (integer? r) (>= l 0) (<= l r) (<= r (string-length str)))
                  (substring str l r)
                  'invalid)))
        ;(println "substr")
        ;(println v)
        v))

    (define/public (concat pos left right)
      (let (( v (if (and (string? left) (string? right))
          (send this basic-binary string-append left right)
          'invalid)))
        ;(println "concat")
        ;(println v)
        v))

    (define/public (date-diff pos left right)
      (date-op (left right) (date-subtract left right)))

    (define/public (date-compare pos left right)
      (date-op (left right) 
               (let ((di1 (val (cons 'di1 pos) boolean?))
                     (di2 (val (cons 'di2 pos) boolean?))
                     (di3 (val (cons 'di3 pos) boolean?)))
                 (if (and (not di1) (not di2) (not di3))
                     'invalid
                     (send this basic-binary
                           (if di1
                               (if di2
                                   (if di3
                                       date-le
                                       date-ge)
                                   (if di3
                                       date-gt date-lt))
                               date-equal) left right)))))

    (define/public (date-interval pos left right)
      (date-op (left)
        (if (integer? right)
            (let ((d (copy-date left))
                  (di1 (val (cons 'di1 pos) boolean?))
                  (di2 (val (cons 'di2 pos) boolean?))
                  (di3 (val (cons 'di3 pos) boolean?))
                  (di4 (val (cons 'di4 pos) boolean?)))
              (if (and (not di1) (not di2)(not di3))
                  'invalid
                  (send this basic-binary
                        (if di1
                            (if di2
                                (if di3
                                    (if di4 add-seconds add-minutes)
                                    (if di4 add-hours add-days))
                                (if di3
                                    (if di4 add-months add-years)
                                    (if di4 subtract-seconds subtract-minutes)))
                            (if di2
                                (if di3 subtract-hours subtract-days)
                                (if di3 subtract-months subtract-years)))
                        d right))
              d)
            'invalid)))
 
    (define/public (date-extract-bin pos v f)
      (if (vector? v)
          (f v)
          'invalid))

    (define/public (date-extract pos v)
      (date-op (v)
              (let ((de1 (val (cons 'de1 pos) boolean?))
                    (de2 (val (cons 'de2 pos) boolean?))
                    (de3 (val (cons 'de3 pos) boolean?)))
                (date-extract-bin pos v 
                                  (if de1
                                      (if de2
                                          (if de3 extract-seconds extract-minutes)
                                          (if de3 extract-hours extract-days))
                                      (if de2
                                          (if de3 extract-months extract-years)
                                          (if de3 extract-day-of-year extract-day-of-week)))))))
    
    (define/public (general-compare date-op number-op left right)
      (date-case-op (left right) ((date-op left right)) ((number-op left right))))

    (define/public (date-to-epoch pos v)
      (date-op (v)
          (send this basic-unary extract-epoch v)))

    (define/public (date-from-epoch pos v)
      (if (integer? v)
          (send this basic-unary extract-date-from-epoch v)
          'invalid))

    (define/public (like pos lhs rhs)
      (println "like")
      (println "rhs")
      (println rhs)
      (println "lhs")
      (println lhs)
      (if (and (string? lhs) (string? rhs))
          (let ((m1 (val (cons 'li1 pos) boolean?))
                (m2 (val (cons 'li2 pos) boolean?)))
            ; (when (and (not m1) (not m2)) (begin (println rhs) (println "about to call like-constant")))
            (for/all ([r rhs #:exhaustive])
              (if (and (not m1) (not m2))
                  ; (println r)
                  (if (not (symbolic? r))   
                      (like-constant lhs r)
                      'invalid)
                  (let ((expr
                         (if m1
                             (if m2
                                 (string-suffix? lhs rhs)
                                 (string-prefix? lhs rhs))
                             (string-contains? lhs rhs))))
                    (println "expr")
                    (println expr)
                    (println "expr")
                    expr))))
          'invalid))

    (define/public (trim pos str)
     (let ((s1 (val (cons 'b str) string?))
            (s2 (val (cons 'd str) string?))
            (s3 (val (cons 'a str) string?)))
        (if (and (equal? str (string-append s1 s2 s3))
                 (regexp-match-exact? #rx"[ ]*" s1)
                 (regexp-match-exact? #rx"[ ]*" s3)
                 (regexp-match-exact? #rx"[A-Za-z0-9_:.,?']+[A-Za-z0-9_:.,?' ]*[A-Za-z0-9_:.,?']+" s2))
              s2
            'invalid)))
      
    (define/public (get-digits pos str)
      (let ((s1 (val (cons 'b str) string?))
            (s2 (val (cons 'd str) string?))
            (s3 (val (cons 'a str) string?)))
        (if (and (equal? str (string-append s1 s2 s3))
                 (regexp-match-exact? #rx"[A-Za-z]*" s1)
                 (regexp-match-exact? #rx"[A-Za-z]*" s3)
                 (regexp-match-exact? #rx"[0-9]+" s2))
            s2
            'invalid)))))

(define compound-processor%
  (class object%
    (init children [extras '()] [ordering-function '()])

    (super-new)

    (init-field [extra-functions extras]
                [processors children]
                [ordering ordering-function])

    (define/public (extra-f)
      extra-functions)
    
    (define/public (get-ordering-function)
      ordering)
    
    (define/public (constant v)
      (for/list ([p processors])
        (send p constant v)))

    (define/public (basic-unary f value)
      (for/list ([p processors] [v value])
        (send p basic-unary f v)))

    (define/public (basic-binary f left right)
      (for/list ([p processors] [l left] [r right])
        (send p basic-binary f l r)))
    
    (define/public (is-null-v? mb vs pos)
      (for/list ([p processors] [v vs])
        (send p is-null-v? mb v pos)))
      
    (define/public (is-null? pos)
      (for/list ([p processors])
        (send p is-null? pos)))

    (define/public (in pos type-f)
      (for/list ([p processors])
        (send p in pos type-f)))

    (define/public (in-v pos v type-f)
      (for/list ([p processors])
        (send p in-v pos v type-f)))
    
    (define/public (symbolic pos type)
      (for/list ([p processors])
        (send p symbolic pos type)))

    (define/public (basic-num-functions pos v)
      (for/list ([p processors] [vs v])
        (send p basic-num-functions pos vs)))

    (define/public (basic-math pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p basic-math pos l r)))
    
    (define/public (index-of pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p index-of pos l r)))
        
    (define/public (date-compare pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p date-compare pos l r)))
    
    (define/public (compare-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p compare-to pos l r)))

    (define/public (compare-to-str pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p compare-to-str pos l r)))

    (define/public (logic-op-not pos v)
      (for/list ([p processors] [vs v])
        (send p logic-op-not pos vs)))
    
    (define/public (if-then-else cases left right)
      (for/list ([p processors] [case cases] [l left] [r right])
        (send p if-then-else case l r)))
    
    (define/public (logic-op pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p logic-op pos l r)))
    
    (define/public (strlength pos strs)
      (for/list ([p processors] [str strs])
        (send p strlength pos str)))
    
    (define/public (substr strs left right)
      (for/list ([p processors] [str strs] [l left] [r right])
        (send p substr str l r)))

    (define/public (concat pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p concat pos l r)))

    (define/public (get-digits pos strs)
      (for/list ([p processors] [s strs])
        (send p get-digits pos s)))

    (define/public (trim pos strs)
      (for/list ([p processors] [s strs])
        (send p trim pos s)))

    (define/public (date-diff pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p date-diff pos l r)))
    
    (define/public (date-interval pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p date-interval pos l r)))

    (define/public (like pos left right)
      (println "c like")
      (for/list ([p processors] [l left] [r right])
        (send p like pos l r)))

    (define/public (date-extract pos v)
     (for/list ([p processors] [vs v])
        (send p date-extract pos vs)))
    
    (define/public (date-to-epoch pos v)
    (for/list ([p processors] [vs v])
        (send p date-to-epoch pos vs)))

    (define/public (date-from-epoch pos v)
      (for/list ([p processors] [vs v])
        (send p date-from-epoch pos vs)))

    (define/public (set-seconds-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-seconds-to pos l r)))

    (define/public (set-minutes-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-minutes-to pos l r)))
    
    (define/public (set-hours-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-hours-to pos l r)))
    
    (define/public (set-days-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-days-to pos l r)))

    (define/public (set-months-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-months-to pos l r)))

    (define/public (set-years-to pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p set-years-to pos l r)))

    (define/public (set-to-first-day-of-month pos v)
       (for/list ([p processors] [vs v])
        (send p set-to-first-day-of-month pos vs)))

    (define/public (set-to-last-day-of-month pos v)
       (for/list ([p processors] [vs v])
        (send p set-to-last-day-of-month pos vs)))

    (define/public (set-to-first-month pos v)
       (for/list ([p processors] [vs v])
        (send p set-to-first-month pos vs)))

    (define/public (set-to-last-month pos v)
      (for/list ([p processors] [vs v])
        (send p set-to-last-month pos vs)))

    (define/public (set-to-next-day pos v)
     (for/list ([p processors] [vs v])
        (send p set-to-next-day pos vs)))

    (define/public (set-to-previous-day pos v)
      (for/list ([p processors] [vs v])
        (send p set-to-previous-day pos vs)))

    (define/public (set-to-next-month pos v)
     (for/list ([p processors] [vs v])
        (send p set-to-next-month pos vs)))

    (define/public (general-compare date-op number-op left right)
      (for/list ([p processors] [l left] [r right])
        (send p general-compare date-op number-op l r)))

    (define/public (set-to-previous-month pos v)
      (for/list ([p processors] [vs v])
        (send p set-to-previous-month pos vs)))

    (define/public (is-in-current-month pos v)
     (for/list ([p processors] [vs v])
        (send p is-in-current-month pos vs)))

    (define/public (is-in-last-x-months pos left right)
      (for/list ([p processors] [l left] [r right])
        (send p is-in-last-x-months pos l r)))

    (define/public (aggregate-op pos type vs op is-average)
      (for/list ([p processors] [v vs])
        (send p aggregate-op pos type v op is-average)))

    (define/public (aggregate pos type vs)
      (for/list ([p processors] [v vs])
        (send p aggregate pos type v)))))
      

(define aggregating-processor%
  (class compound-processor%

    (super-new)

    (inherit-field processors)

    (define (aggregate pos type v)
      (let* ((stuff
              (if (eq? type 'string)
                  (list string-append #f)
                  (let ((v1 (val (cons 1 pos) boolean?))
                        (v2 (val (cons 2 pos) boolean?)))
                    (list
                     (cond ((and v1 v2) min)
                           ((and v1 (not v2)) max)
                           (#t +))
                     (and (not v1) (not v2))))))
             (op (car stuff))
             (is-average (cadr stuff)))
        (send this aggregate-op pos type v op is-average)))
    
    (define (aggregate-op pos type v op is-average)
      (if (member 'invalid v)
          (for/list ([x v])
            'invalid)
          (let ((val
                 (for/fold ([f (cadr v)])
                           ([e (cddr v)])
                   (op f e))))
            (cons
             (let ((v1 (op (car v) val)))
               (if is-average (/ v1 (length v)) v1))
             (let ((v1 (if is-average (/ val (length (cdr v))) val))) 
               (for/list ([x (cdr v)])
                 v1))))))

    (override aggregate-op)
    (override aggregate)))

(define (do-all type ops size pos p f)
  (let ((extras (send p extra-f)))
    (let ((do-custom
           (lambda (type)
             (when (hash-has-key? extras type)
               (let ((fs (hash-ref extras type)))
                 (for ([op fs])
                   (hash-set! extras type (remove op fs))
                   (op size pos p f)
                   (hash-set! extras type fs)))))))
      (do-custom type)
      (do-custom 'any)
      (for ([op ((send p get-ordering-function) ops)])
        (op size pos p f)))))

(define (do-all-str size pos p f)
  (do-all 'string
   (list do-in-str do-str-aggregate do-strv do-if-then-str do-concat do-substring do-get-digits do-trim)
   size pos p f))

(define (do-all-int-no-date size pos p f)
  (do-all 'number
    (list do-in-int do-int-aggregate do-intv do-basic-math do-if-then-int do-basic-num-functions)
   size pos p f))

(define (do-all-int size pos p f)
  (do-all 'number
     (list do-in-int do-int-aggregate do-intv do-basic-math do-if-then-int do-basic-num-functions do-date-diff do-date-extract do-date-to-epoch)
;    (list do-in-int do-intv do-basic-math do-if-then-int do-basic-num-functions do-date-diff do-date-extract do-date-to-epoch)
;    (list do-in-int do-int-aggregate do-intv do-basic-math do-if-then-int do-basic-num-functions)
   size pos p f))

(define (do-all-any size pos p f)
  (do-all-bool size pos p f)
  (do-all-int size pos p f)
  (do-all-str size pos p f))
  
(define (do-math-int size pos p f)
  (do-all 'number
   (list do-in-int do-int-aggregate do-if-then-int do-basic-math do-index-of do-length do-basic-num-functions)
   size pos p f))

(define (do-all-date size pos p f)
  (do-all 'vector
          ; (list do-in-date do-date-interval)
          (list do-in-date do-date-from-epoch do-date-interval)
          size pos p f))


(define (do-all-bool size pos p f)
  (do-all 'boolean
   (list do-compare-to do-compare-to-str do-logic-op do-logic-op-not do-is-null? do-like do-date-compare)
   size pos p f))

(define (do-in-int size pos p f) (f size (send p in pos number?)))

(define (do-in-str size pos p f) (f size (send p in pos string?)))

; dates are represented as vectors
(define (do-in-date size pos p f) (f size (send p in pos vector?)))

(define (do-intv size pos p f) (f size (send p symbolic (cons 'int pos) integer?)))

(define (do-strv size pos p f) (f size (send p symbolic (cons 'str pos) string?)))

(define (do-datev size pos p f) (f size (send p symbolic (cons 'date pos) vector?)))

(define (do-is-null? size pos p f)
  (when (>= size 0)
    (f (- size 1) (send p is-null? pos))))

(define (custom op . children)
  (lambda (size pos p f)
    (letrec ((rec (lambda (i sz cs args)
                    (when (>= sz 0)
                      (if (null? cs)
                          (f sz (apply op p pos args))
                          ((car cs)
                           sz
                           (cons i pos)
                           p
                           (lambda (new-size v)
                             (rec (+ i 1) new-size (cdr cs) (append args (list v))))))))))
      (rec 1 size children '()))))

(define (do-unary-op do-arg op size pos p f)
  ((custom (lambda (p pos new-expr) (dynamic-send p op pos new-expr)) do-arg)
   (- size 2) pos p f))

(define (do-binary-op do-arg1 do-arg2 op size pos p f)
  ((custom (lambda (p pos left-expr right-expr) (dynamic-send p op pos left-expr right-expr)) do-arg1 do-arg2)
   (- size 3) pos p f))

(define (do-ternary-op do-arg1 do-arg2 do-arg3 op size pos p f)
  ((custom (lambda (p pos str start end) (dynamic-send p op str start end)) do-arg1 do-arg2 do-arg3)
   (- size 4) pos p f))

(define (do-get-digits size pos p f)
  (do-unary-op do-all-str 'get-digits size pos p f))

(define (do-trim size pos p f)
  (do-unary-op do-all-str 'trim size pos p f))

(define (do-concat size pos p f)
  (do-binary-op do-all-str do-all-str 'concat size pos p f))

(define (do-basic-math size pos p f)
  (do-binary-op do-math-int do-math-int 'basic-math size pos p f))

(define (do-date-compare size pos p f)
  (do-binary-op do-all-date do-all-date 'date-compare size pos p f))

(define (do-date-diff size pos p f)
  (do-binary-op do-all-date do-all-date 'date-diff size pos p f))

(define (do-date-interval size pos p f)
  (do-binary-op do-all-date do-all-int 'date-interval size pos p f))

(define (do-date-extract size pos p f)
  (do-unary-op do-all-date 'date-extract size pos p f))

(define (do-date-to-epoch size pos p f)
  (do-unary-op do-all-date 'date-to-epoch size pos p f))

(define (do-date-from-epoch size pos p f)
  (do-unary-op do-all-int 'date-from-epoch size pos p f))

(define (do-basic-num-functions size pos p f)
  (do-unary-op do-all-int 'basic-num-functions size pos p f))

(define (do-index-of size pos p f)
  (do-binary-op do-all-str do-all-str 'index-of (- size 2) pos p f))

(define (do-compare-to size pos p f)
  (do-binary-op do-all-int do-all-int 'compare-to size pos p f))

(define (do-compare-to-str size pos p f)
  (do-binary-op do-all-str do-all-str 'compare-to-str size pos p f))

(define (do-logic-op size pos p f)
  (do-binary-op do-all-bool do-all-bool 'logic-op size pos p f))

(define (do-logic-op-not size pos p f)
  (do-unary-op do-all-bool 'logic-op-not size pos p f))

(define (do-if-then-str size pos p f)
  (do-ternary-op do-all-bool do-all-str do-all-str 'if-then-else size pos p f))

(define (do-if-then-int size pos p f)
  (do-ternary-op do-all-bool do-all-int do-all-int 'if-then-else size pos p f))

(define (do-length size pos p f)
  (do-unary-op do-all-str 'strlength size pos p f))

(define (do-substring size pos p f)
  (do-ternary-op do-all-str do-all-int do-all-int 'substr size pos p f))

(define (do-like size pos p f)
  (do-binary-op do-all-str do-all-str 'like size pos p f))

(define (do-aggregate do-all-op type size pos p f)
  (when (> size 0)
    (do-all-op
     (- size 1) (cons 'agg pos) p
     (lambda (size expr)
       (when (> size 0)
         (f (- size 1) (send p aggregate pos type expr)))))))

(define (do-int-aggregate size pos p f)
  (do-aggregate do-all-int-no-date 'integer size pos p f))

(define (do-str-aggregate size pos p f)
  (do-aggregate do-all-str 'string size pos p f))

(define (symbolic? x) 
  (or (union? x) (term? x)))

(define (convert-to-rational v)
  (if (real? v)
      (let ((parts (string-split (~v v) ".")))
        (if (= (length parts) 1)
            v
            (/ (with-input-from-string (apply string-append parts) (lambda () (read)))
               (expt 10 (string-length (cadr parts))))))
      v))

; limit - max size of expressions to search over in terms of primitive operations
; outputs - a list of values per row.  Assumption is output can be only one column
; inputs -  a list of rows such as (6 3 3) (9 6 3)
; symbolic - a symbolic formula that defines the formula which defines our result, which will be ultimately
; be used to create rows that either fit the formula or dont
; white - white list of functions
; black - black list of functions

(define (analyze extra white black limit outputs symbolic . inputs)
  ; goals - number of solutions wanted
  ; models - set of expressions returned by the search 
  (letrec ((z3-engines (new engines% [n 5]))
           (time-limit 30000)
           (start-time (current-inexact-milliseconds))
           (results-channel (make-async-channel 10000))
           (goal 1)
           (models (list))
           (outstanding 0)
           (universals (filter term? (flatten inputs))))
    ; exception handler code checks if we have a list, and if so returns the list, lets other exceptions bubble up, see raise at the bottom
    ; the function where the result is raised as a list
    (with-handlers ([(lambda (v) (pair? v)) (lambda (v) v)])
      ((let ((v (car outputs)))
         (cond ((boolean? v) do-all-bool)
               ((number? v) do-all-int)
               ((vector? v) do-all-date)
               (#t do-all-str)))
       ; do-all-bool/do-all-int etc will be called with limit, an empty list (list) to indicate the root node of the expression tree we are developing
       ; a list of processors which include 1 for printing (doc-processor) and 1 expression processor initialized with inputs per row (cdr inputs discards
       ; first element which is the output), so we now have 1 expression processor per row.
       ; and a lambda function is a callback function that gets invoked on y which is a list of expressions that the expression processor comes up with in the search
       ; Note the first expression in y is discarded in processing for formulas because it reflects the output of the doc processor
       limit (list)
       (new compound-processor%
            [extras extra]
            [ordering-function
             (lambda (ops)
               (append
                (filter (lambda (op) (member op white)) ops)
                (filter (lambda (op) (not (member op (append black white)))) ops)))]
            [children
             (cons
              (new doc-processor%)
              (list
               (new logging-processor%)
               (new aggregating-processor%
                    [children
                     (cons
                      (new expr-processor% [inputs symbolic])
                      (for/list ([input inputs])
                        ; initialize the field inputs in the expr-processor object with current row inputs.  Note the inputs in the following
                        ; line does not refer to the parameter passed into this function
                        (new expr-processor% [inputs input])))])))])
       (lambda (x y)
          (when (null? (apply append (hash-values extra)))
         (set! outstanding (+ outstanding 1))
         (let ((formula
                (for/fold ([formula #t])
                          ([out outputs]
                           [in (cdr (third y))])
                  ; collect up all expressions generated by the expression processor in y (cddr y)
                  ; and create a single condition in formula for a solver to assert on with ANDs linking across rows.
                  (and
                   formula
                   (cond ((eq? out #t) (eq? #t in))
                         ((eq? out #f) (eq? #f in))
                         ((number? out) (and (number? in) (= out in)))
                         (#t (equal? out in)))))))
           ;(println (car y))
           ;(println formula)
           ; solver assertions.  When we have a satisfiable model and we have reached the number of goal (or solutions) we want
           ; raise models, which will then be trapped by the handler code
           (when (and (not (symbolic? formula)) (eq? #t formula))
             (set! models
                   (append
                    models
                    (list (list (car y) (remove-duplicates (cadr y)) (car (third y)) null null)))))
           (send z3-engines solve formula x
             (lambda (formula result)
               ; (when (and (sat? result) (evaluate formula result)) - the  (evaluate formula result) should not be necessary but Z3
               ; has bugs so check.
               (when (and (not (exn? result)) (sat? result) (eq? #t (evaluate formula result)))
                 (if (null? universals)
                     (set! models
                           (append
                            models
                            (list (list (car y) (remove-duplicates (cadr y)) (car (third y)) result null))))
                     (let* ((symbolic (hash->list (model result)))
                            (constraints (filter (lambda (p) (not (member (car p) universals))) symbolic))
                            (guard
                             (letrec ((g (lambda (ss)
                                           (if (null? ss)
                                               #t
                                               (and
                                                (equal? (caar ss) (cdar ss))
                                                (g (cdr ss)))))))
                               (g symbolic)))
                            (negated-formula
                             (and 
                              guard
                              (not formula)))
                            (solver (z3)))
                       (print universals)
                       (println guard)
                       (solver-clear solver)
                       (solver-assert solver (list negated-formula))
                       (let ((negated-result (solver-check solver)))
                         (solver-shutdown solver)
                         (when (not (sat? negated-result))
                           (set! models
                                 (append
                                  models
                                  (list (list (car y) (remove-duplicates (cadr y)) (caddr y) result null)))))))))))
           (when (> (length models) goal)
             (raise models)))))))
    (send z3-engines drain)
    models))

(define (aggregate white black limit results symbolic . inputs)
  (let ((solver (current-solver))
        (goal 2)
        (models (list))
        (start-time (current-seconds))
        (i 0))
    (with-handlers ([(lambda (v) (pair? v)) (lambda (v) v)])
      ((cond ((boolean? (car results)) do-all-bool)
             ((number? (car results)) do-all-int)
             (#t do-all-str))
       limit (list)
       (new compound-processor%
            [children
             (cons
              (new doc-processor%)
              (for/list ([input inputs])
                (new expr-processor% [inputs input])))])
       (lambda (x y)
         (when (and (null? (filter void? y)) (null? (filter (lambda (x) (eq? x 'invalid)) y)))
           (solver-clear solver)
           (set! i (+ i 1))
           (let* ((op (cond ((boolean? (car results))
                             (let ((v (constant 'agg boolean?)))
                               (lambda (x y) (if v (and x y) (or x y)))))
                            ((number? (car results))
                             (let ((v1 (constant 'agg1 boolean?))
                                   (v2 (constant 'agg2 boolean?)))
                               (if v1 (if v2 + max) min))) 
                            (#t string-append)))
                   (strop (cond ((boolean? (car results))
                             (let ((v (constant 'agg boolean?)))
                               (lambda (x y) (if v ('and) ('or)))))
                            ((number? (car results))
                             (let ((v1 (constant 'agg1 boolean?))
                                   (v2 (constant 'agg2 boolean?)))
                               (if v1 (if v2 '+ 'max) 'min))) 
                            (#t 'string-append)))
                  (formula
                   (for/fold ([formula null])
                             ([result results]
                              [in (cdr y)])
                      (if (null? formula)
                         (list in (equal? in result))
                         (let ((f (op (car formula) in)))
                           (list f (and (equal? f result) (cadr formula))))))))
             (solver-assert solver (cdr formula))
             (let* ((result (solver-check solver)))
               (when (and (sat? result) (evaluate formula result))
                 (set! models (cons (list (car y) result strop) models))
                 (set! goal (- goal 1))
                 (when (or (= goal 0) (> 60 (- current-seconds start-time)))
                   (raise models))))))))
      (list))))

(define (render solution)
  (let* ((tree (list-ref solution 0))
         (model (list-ref solution 3))
         (agg (list-ref solution 4)))
    (cons (if (not (list? agg)) (evaluate agg model) 'notagg)
    (letrec ((print-tree
              (lambda (node)
                (cond [(list? node) (map print-tree node)]
                      [(symbolic? node)     ; symbolic variables
                       (let ((result (evaluate node model)))
                         (if (union? result)
                             (map cdr (union-contents result))
                             result))]
                      [#t node]))))
      (list (print-tree tree) (print-tree (list-ref solution 1)))))))

(define (testListMembers l1 l2)
  (if (equal? l2 '())
      '()
     (begin
       (assert (list? (member (car l2) (flatten l1))))
       (testListMembers l1 (cdr l2)))))

(define (check-operation l op)
  ;(println l)
  (when (list? l)
    (testListMembers l op)))

; func is the function to use - analyze or aggregates
; limit determines the total num of expressions it can use
; op is the top level node for the expression tree (for now) - to check if we got the right operation
(define (test func op white raw-black limit raw-outputs symbolic raw-inputs)
  (test-int  func op (hash) white raw-black 1 limit raw-outputs symbolic raw-inputs))

(define (test-int func op custom white raw-black start limit raw-outputs symbolic raw-inputs)
   ; convert all reals to rational numbers in case we have any
  (let ((inputs (map (lambda(x) (map convert-to-rational x)) raw-inputs))
        (outputs (map convert-to-rational raw-outputs))
        (black (flatten (map get-function-mappings raw-black))))
    (letrec ((try-depth
              (lambda(v)
                (let ((out (apply func custom white black v outputs symbolic inputs)))
                  (if (not (null? out))
                      (map render out)
                      (if (< v limit)
                          (try-depth (+ 1 v))
                          '()))))))
      (let ((o (try-depth start)))
        (check-operation o op)
        (sort o
          (lambda (x y)
            (let ((l (lambda (r) (length (flatten (second r))))))
              (< (l x) (l y)))))))))

(define func_to_procs (make-hash))

(hash-set! func_to_procs 'or (list do-logic-op))
(hash-set! func_to_procs 'and (list do-logic-op))
(hash-set! func_to_procs 'not (list do-logic-op-not))
(hash-set! func_to_procs '= (list do-compare-to))
(hash-set! func_to_procs '<= (list do-compare-to))
(hash-set! func_to_procs '>= (list do-compare-to))
(hash-set! func_to_procs '< (list do-compare-to))
(hash-set! func_to_procs '> (list do-compare-to))
(hash-set! func_to_procs 'concat (list do-concat))
(hash-set! func_to_procs '+ (list do-basic-math))
(hash-set! func_to_procs '- (list do-basic-math))
(hash-set! func_to_procs '/ (list do-basic-math))
(hash-set! func_to_procs '* (list do-basic-math))
(hash-set! func_to_procs 'quotient (list do-basic-num-functions))
(hash-set! func_to_procs 'remainder (list do-basic-num-functions))
(hash-set! func_to_procs 'abs  (list do-basic-num-functions))
(hash-set! func_to_procs 'ceiling  (list do-basic-num-functions))
(hash-set! func_to_procs 'floor  (list do-basic-num-functions))
(hash-set! func_to_procs 'truncate  (list do-basic-num-functions))
(hash-set! func_to_procs 'sign  (list do-basic-num-functions))
(hash-set! func_to_procs 'if (list do-if-then-str do-if-then-int))
(hash-set! func_to_procs 'substring (list do-substring))
(hash-set! func_to_procs 'index-of (list do-index-of))
(hash-set! func_to_procs 'length (list do-length))
(hash-set! func_to_procs '== (list do-compare-to-str))
(hash-set! func_to_procs 'in (list do-in-int do-in-str))
(hash-set! func_to_procs 'is-null (list do-is-null?))
(hash-set! func_to_procs 'is-not-null (list do-is-null?))

(define (get-function-mappings func)
  (hash-ref func_to_procs func))

(provide aggregating-processor% doc-processor% compound-processor% expr-processor% analyze render aggregate test test-int val custom do-basic-num-functions do-logic-op-not do-in-str do-concat do-logic-op do-all-any do-all-int do-all-str do-all-bool do-strv do-if-then-int do-intv do-basic-num-functions do-index-of do-basic-math do-substring do-get-digits do-trim do-length do-compare-to clear-vals!)
