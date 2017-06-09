#lang rosette

(require web-server/servlet
         web-server/servlet-env)
(require "interp-enumerate.rkt")

; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  (and (exists-binding? 'input bindings)
       (exists-binding? 'output bindings)))

; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (list (extract-binding/single 'input bindings)
        (extract-binding/single 'output bindings)))

(define (start request)
  (print request)
  (cond [(can-parse-post? (request-bindings request))
         (let ((v (map render (analyze 5 (parse-post (request-bindings request))))))
           (print v))
           ]
        [else
         (print 'error)])
  (response/xexpr `(html (head (title "My Blog"))
                         (body
                          (h1 "My Blog")))))
 
(serve/servlet start
               #:servlet-path "/synthesis"
               #:extra-files-paths
               (list
                (build-path ".")))
