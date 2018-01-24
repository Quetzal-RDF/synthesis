#lang rosette

(current-bitwidth #f)

(define s1 "€")

(define-symbolic s2 string?)

(solve (assert (string-contains? s2 s1)))

(define r1 #rx"[0-9]*")

(define r2 #rx"[A-Z]*")

(solve (assert (and (> (string-length s2) 0) (regexp-match-exact? r2 s2))))

(define-symbolic b1 boolean?)

(solve (assert (and (> (string-length s2) 0)
                    (if b1
                        (regexp-match-exact? r1 s2)
                        (regexp-match-exact? r2 s2)))))

(solve (assert (and (> (string-length s2) 0)
                    (let ((rs (if b1 r1 r2)))
                      (for/all ([r rs #:exhaustive])
                        (regexp-match-exact? r s2))))))

(define-symbolic i1 integer?)

(solve (assert (equal? (integer->string i1) "1")))

(solve (assert (= 1 (string->integer s2))))

(solve (assert (and (> (string-length s2) 0) (regexp-match-exact? (if b1 r1 r2) s2))))
