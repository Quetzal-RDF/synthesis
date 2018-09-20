#lang rosette

(define (constrain rows f-merge-row f-merge-col f-combine-cols)
  (letrec ((constraints
            (lambda (ll)
              (let ((next
                     (letrec ((stitch
                               (lambda (l1 l2)
                                 (if (null? (cdr l1))
                                     (f-combine-cols (car l1) (car l2))
                                     (f-merge-col (f-combine-cols (car l1) (car l2))
                                                   (stitch (cdr l1) (cdr l2)))))))
                       (stitch (car ll) (cadr ll)))))
                (if (null? (cddr ll))
                    next
                    (f-merge-row next (constraints (cdr ll))))))))
    (constraints rows)))

(define (minimize-row-differences rows)
  (constrain
   rows
   append
   (lambda (c1 c2) (if (list? c2) (cons c1 c2) (list c1 c2)))
   (lambda (c1 c2) (if (number? c1) (abs (- c1 c2)) 0))))

(define (make-row-conditions-different rows)
  (constrain
   rows
   (lambda (a b) (and a b))
   (lambda (a b) (or a b))
   (lambda (a b) (not (equal? a b)))))

(define (balance rows)
  (if (or (null? rows) (null? (car rows)))
      '()
      (cons
       (abs (- (apply + (map (lambda (e) (if e 1 0)) (map car rows))) (/ (length rows) 2)))
       (balance (map cdr rows)))))

(provide minimize-row-differences make-row-conditions-different balance)