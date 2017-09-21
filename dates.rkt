#lang rosette

(current-bitwidth #f)

(require racket/date)

(define days-of-month (make-hash '((1 31) (2 28) (3 31) (4 30) (5 31) (6 30) (7 31) (8 31) (9 30) (10 31) (11 30) (12 31))))

(define cum-days-of-month (make-hash '((1 31) (2 59) (3 90) (4 120) (5 151) (6 181) (7 212) (8 243) (9 273) (10 304) (11 334) (12 365))))

(define days-of-week (make-hash '((1 0) (2 1) (3 2) (4 3) (5 4) (6 5) (0 6))))

(define (new-date)
  (make-vector 6))

(define (create-date seconds minutes hours days months years)
  (let ((d (new-date)))
    (set-field d "seconds" seconds)
    (set-field d "minutes" minutes)
    (set-field d "hours" hours)
    (set-field d "days" days)
    (set-field d "months" months)
    (set-field d "years" years)
    d))

(define (set-field date field value)
  (cond [(equal? field "seconds") (vector-set! date 0 value)]
        [(equal? field "minutes") (vector-set! date 1 value)]
        [(equal? field "hours") (vector-set! date 2 value)]
        [(equal? field "days") (vector-set! date 3 value)]
        [(equal? field "months") (vector-set! date 4 value)]
        [(equal? field "years") (vector-set! date 5 value)]))

(define (get-field date field)
  (cond [(equal? field "seconds") (vector-ref date 0)]
        [(equal? field "minutes") (vector-ref date 1)]
        [(equal? field "hours") (vector-ref date 2)]
        [(equal? field "days") (vector-ref date 3)]
        [(equal? field "months") (vector-ref date 4)]
        [(equal? field "years") (vector-ref date 5)]))

(define (make-date date)
  (let ((d (new-date)))
    (set-field d "seconds" (get-field date "seconds"))
    (set-field d "minutes" (get-field date "minutes"))
    (set-field d "hours" (get-field date "hours"))
    (set-field d "days" (get-field date "days"))
    (set-field d "months" (get-field date "months"))
    (set-field d "years" (get-field date "years"))
    d))

; add intervals of various kinds to a date
(define (add-seconds date s)
  (assert (< s 60))
  (let ((p (+ (get-field date "seconds") s)))
    (if (< p 60)
        (set-field date "seconds" p)
        (begin
          (set-field date "seconds" (- p 60))
          (add-minutes date 1)))))

(define (add-minutes date m)
  (assert (< m 60))
  (let ((p (+ (get-field date "minutes") m)))
    (if (< p 60)
        (set-field date "minutes" p)
        (begin
          (set-field date "minutes" (- p 60))
          (add-hours date 1)))))

(define (add-hours date h)
  (assert (< h 24))
  (let ((p (+ (get-field date "hours") h)))
    (if (< p 24)
        (set-field date "hours" p)
        (begin
          (set-field date "hours" (- p 24))
          (add-days date 1)))))

(define (is-leap date)
  (let ((years (get-field date "years")))
    (if (= 0 (modulo years 100))
        (= 0 (modulo years 400))
        (= 0 (modulo years 4)))))

(define (num-days-in-month date)
  (let ((months (get-field date "months")))
    (if (and (equal? 2 months) (is-leap date))
        29
        (first (hash-ref days-of-month months)))))

(define (add-days date d)
  (assert (<= d 28))  ; make sure days cannot span more than 1 month
  (let ((p (+ (get-field date "days") d)))
    (if (<= p (num-days-in-month date))
        (set-field date "days" p)
        (begin
          (set-field date "days" (- p (num-days-in-month date)))
          (add-months date 1)))))


(define (check-valid-day date)
  (when (< (num-days-in-month date) (get-field date "days"))
    (set-field date "days" (num-days-in-month date))))


(define (add-months date m)
  (assert (<= m 12))
  (let ((p (+ (get-field date "months") m)))
    (if (<= p 12)
        (set-field date "months" p)
        (begin
          (set-field date "months" (- p 12))
          (add-years date 1)))
    (check-valid-day date)))

(define (add-years date y)
  (assert (< y 100))
  (set-field date "years" (+ y (get-field date "years"))))

; subtract intervals of various kinds to a date
(define (subtract-seconds date s)
  (assert (< s 60))
  (let ((p (- (get-field date "seconds") s)))
    (if (> p 0)
        (set-field date "seconds" p)
        (begin
          (set-field date "seconds" (+ p 60))
          (subtract-minutes date 1)))))

(define (subtract-minutes date m)
  (assert (< m 60))
  (let ((p (- (get-field date "minutes") m)))
    (if (> p 0)
        (set-field date "minutes" p)
        (begin
          (set-field date "minutes" (+ p 60))
              (subtract-hours date 1)))))

(define (subtract-hours date h)
  (assert (< h 24))
  (let ((p (- (get-field date "hours") h)))
    (if (> p 0)
        (set-field date "hours" p)
        (begin
          (set-field date "hours" (+ p 24))
          (subtract-days date 1)))))

(define (num-days date m)
  (if (and (equal? 2 m) (is-leap date))
      29
      (first (hash-ref days-of-month m))))


(define (subtract-one-month date)
  (if (equal? (get-field date "months") 1) 12 (- (get-field date "months") 1)))


(define (subtract-days date d)
  (assert (<= d 28))  ; make sure days cannot span more than 1 month
  (let ((p (- (get-field date "days") d)))
    (if (> p 0)
        (set-field date "days" p)
        (begin
          (set-field date "days" (+ (num-days  date (subtract-one-month date)) p)) ;p is negative already, so take the number of days in prev month and add those
            (subtract-months date 1)))))

(define (subtract-months date m)
  (assert (<= m 12))
  (let ((p (- (get-field date "months") m)))
    (if (> p 0)
        (set-field date "months" p)
        (begin
          (set-field date "months" (+ p 12))
          (subtract-years date 1)))
    (check-valid-day date)))

(define (subtract-years date y)
  (assert (< y 100))
  (set-field date "years" (- (get-field date "years") y)))

; inverse of above, from http://howardhinnant.github.io/date_algorithms.html#days_from_civil.  Days from Jan 1 1970 for Unix epoch time computations
(define (civil_from_days z)
     (letrec ((days (+ z 719468))
         (era (if (>= days 0) (quotient days 146097) (quotient (- days 146096) 146097)))
         (doe (- days (* era 146097)))
         (yoe (quotient (- (+ doe (quotient doe 36524)) (quotient doe 1460) (quotient doe 146096)) 365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (- (* 365 yoe) (quotient yoe 100)) (quotient yoe 4))))               
         (mp (quotient (+ (* 5 doy) 2) 153))                                   
         (d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9))))
      (if (<= m 2) (list (+ y 1) m d)
            (list y m d))))

; extraction of date from epoch
(define (date-from-epoch t)
 (let* ((s (modulo t 86400))
       (d (civil_from_days (floor (/ t 86400))))
       (hours (quotient s (* 60 60)))
       (rem (modulo s (* 60 60)))
       (minutes (quotient rem 60))
       (seconds (modulo s 60))
       (date (new-date)))
  (begin
    (set-field date "years" (list-ref d 0))
    (set-field date "months" (list-ref d 1))
    (set-field date "days" (list-ref d 2))
    (set-field date "hours" hours)
    (set-field date "minutes" minutes)
    (set-field date "seconds" seconds)) date))


; http://howardhinnant.github.io/date_algorithms.html#days_from_civil.  Days from Jan 1 1970 for Unix epoch time computations
(define (get-days-since-civil date)
  (let* ((months (get-field date "months"))
         (years (get-field date "years"))
         (days (get-field date "days"))
         (y (if (<= months 2) (- years 1) years))
           (era (if (>= y 0) (quotient y 400) (quotient (- y 399) 400)))
           (yoe (- y (* era 400)))
           (z (if (> months 2) -3 9))
           (doy (- (+ (quotient (+ (* 153 (+ months z)) 2) 5) days) 1))
           (doe (- (+ (* yoe 365) (quotient yoe 4) doy) (quotient yoe 100))))
    (- (+ (* era 146097) doe) 719468)))

; extract epoch from date
(define (extract-epoch date)
  (* (get-days-since-civil date) 86400))

; date comparison/subtraction operations
(define (date-subtract date1 date2)
  (let ((e1 (extract-epoch date1))
        (e2 (extract-epoch date2)))
    (- e1 e2)))

(define (date-ge date1 date2)
  (let ((e1 (extract-epoch date1))
        (e2 (extract-epoch date2)))
    (- e1 e2)))

(define (date-compare op date1 date2)
  (let ((e1 (extract-epoch date1))
        (e2 (extract-epoch date2)))
    (op e1 e2)))

(define (extract-seconds date)
  (get-field date "seconds"))

(define (extract-minutes date)
  (get-field date "minutes"))

(define (extract-hours date)
  (get-field date "hours"))

(define (extract-days date)
  (get-field date "days"))

(define (extract-months date)
  (get-field date "months"))

(define (extract-years date)
  (get-field date "years"))


(define (extract-day-of-year date)
  (let ((months (extract-months date))
        (days (extract-days date)))
    (if (= months 1)
        days
        (let ((m (- months 1)))
          (if (and (> months 2) (is-leap date))
              (+ (first (hash-ref cum-days-of-month m)) days 1)
              (+ (first (hash-ref cum-days-of-month m)) days))))))
      
; from wikipedia, Zeller’s algorithm, last step converts to postgresql's notion of week days
(define (extract-day-of-week date)
  (let* ((months (extract-months date))
         (years (extract-years date))
         (days (extract-days date))
         (Y (if (or (= months 1) (= months 2)) (- years 1) years))
         (y (modulo years 100))
         (c (quotient years 100))
         (m (cond [(= months 1) 13]
                 [(= months 2) 14]
                 [#t months])))
    (let ((dow
           (modulo (- (+ days
                         (floor (* 13 (/ (+ 1 m) 5)))
                         y
                         (floor (/ y 4))
                         (floor (/ c 4)))
                      (* 2 c))
                   7)))
      (first (hash-ref days-of-week dow)))))

(provide new-date add-seconds add-minutes add-hours add-days add-months add-years subtract-seconds subtract-minutes subtract-hours subtract-days subtract-months subtract-years
         set-field get-field is-leap create-date date-from-epoch date-subtract date-compare extract-day-of-year extract-epoch extract-day-of-week)