#lang rosette

(require "./interp-enumerate.rkt")

(println (aggregate 3 '(1 2 3) '(1) '(2) '(3)))

(println (aggregate 3 '(2 3 4) '(1) '(2) '(3)))

(println (aggregate 3 '("aa" "aabb" "aabbcc") '("a") '("b") '("c")))

(println (aggregate 3 '("aa" "aabb" "aabbcc") '("aax") '("bbx") '("ccx")))

(println (aggregate 3 '(30 30) '(30 4) '(5 7)))

(println (aggregate 3 '(9 27) '(10 .1) '(20 .1)))

(println (aggregate 5 '(9.45 27.05) '(10 .1 .05) '(20 .2 .1)))
