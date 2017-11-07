#lang rosette

(require web-server/servlet
         web-server/servlet-env)
(require "interp-enumerate.rkt")
(require "expression-writer.rkt")
(require "utils.rkt")
(require "expression-lexer.rkt")
(require "parse.rkt")
(require "custom.rkt")

(require json)
(require web-server/dispatch)

(define (status->message status)
  (case status
    [(100) #"Continue"]
    [(101) #"Switching Protocols"]
    [(200) #"OK"]
    [(201) #"Created"]
    [(202) #"Accepted"]
    [(203) #"Non-Authoritative Information"]
    [(204) #"No Content"]
    [(205) #"Reset Content"]
    [(206) #"Partial Content"]
    [(300) #"Multiple Choices"]
    [(301) #"Moved Permanently"]
    [(302) #"Found"]
    [(303) #"See Other"]
    [(304) #"Not Modified"]
    [(305) #"Use Proxy"]
    [(307) #"Temporary Redirect"]
    [(400) #"Bad Request"]
    [(401) #"Unauthorized"]
    [(402) #"Payment Required"]
    [(403) #"Forbidden"]
    [(404) #"Not Found"]
    [(405) #"Method Not Allowed"]
    [(406) #"Not Acceptable"]
    [(407) #"Proxy Authentication Required"]
    [(408) #"Request Timeout"]
    [(409) #"Conflict"]
    [(410) #"Gone"]
    [(411) #"Length Required"]
    [(412) #"Precondition Failed"]
    [(413) #"Request Entity Too Large"]
    [(414) #"Request-URI Too Long"]
    [(415) #"Unsupported Media Type"]
    [(416) #"Requested Range Not Satisfiable"]
    [(417) #"Expectation Failed"]
    [(500) #"Internal Server Error"]
    [(501) #"Not Implemented"]
    [(502) #"Bad Gateway"]
    [(503) #"Service Unavailable"]
    [(504) #"Gateway Timeout"]
    [(505) #"HTTP Version Not Supported"]
    [else #""]))

; parse-post: reads in the JSON object, produces a hashtable
(define (parse-post response)
  (bytes->jsexpr response))

(define (parse-column-metadata p)
  (letrec ((m (read (open-input-string (hash-ref p 'columnMetadata)))))
    (for/list ([i m])
      (let ((type (car (list-ref i 3)))
            (colName (cadr i)))
	(if (= type 2)
	    (for/vector ([elt '("s" "m" "h" "dy" "mn" "yr")])
	       (val (make-col-name-for-date colName elt) integer?))
	  (val (make-col-name colName)
	       (cond [(= type 1) integer?]
		     [(= type 3) string?]
		     [(= type 4) boolean?]
		     [(= type 5) real?]
		     [#t string?])))))))

(define (json-response-maker status headers body)
  (println (jsexpr->string body))
  (response/full status
            (status->message status)
            (current-seconds)
            #"application/json; charset=utf-8"
            headers
            (list (string->bytes/utf-8 (jsexpr->string body)))))


(define (synthesize request)
  (letrec ((parsed (parse-post (request-post-data/raw request)))
           (input (read (open-input-string (hash-ref parsed 'inputStr))))
           (output (read (open-input-string (hash-ref parsed 'outputStr))))
           (query (lex (open-input-string (hash-ref parsed 'query))))
           (columnMetadata (read (open-input-string (hash-ref parsed 'columnMetadata))))
           (symbolics (parse-column-metadata parsed)))
    (println input)
    (println output)
    (println query)
    (println columnMetadata)
    (println symbolics)
    ; analyze returns multiple solutions, support only 1 solution for now
    (let ((result (car (apply analyze-custom query output symbolics input))))
      (let ((h (hasheq 'html (to-html (cadr result) columnMetadata) 'json (jsonify (cadr result) columnMetadata))))
        (send/back
         (json-response-maker 202 '() h))))))

(define (columns columnMetadata)
  (map cadr columnMetadata))

(define (parse request)
   (let* ((parsed request)
          (parsed (parse-post (request-post-data/raw request)))
            (query (lex (open-input-string (hash-ref parsed 'queryDef))))
            (columnMetadata (read (open-input-string (hash-ref parsed 'columnMetadata))))
            (symbolics (parse-column-metadata parsed))
            (cols (columns columnMetadata))
            (parser (apply make-parser cols))
            (result (parser query)))
     (println query)
     (println columnMetadata)
     (println symbolics)
     (println cols)
     (println result)

     (let ((h (if (= 1 (length result))
                  (hasheq 'html (to-html (car result) columnMetadata)
                          'json (jsonify (car result) columnMetadata))
                  (let ((sample (generate-data query symbolics columnMetadata)))
                    (hasheq 'html (string-join (map (lambda (x) (to-html x columnMetadata)) result))
                            'table sample)))))
       (println h)
       (send/back
        (json-response-maker 202 '() h)))))

(define sampleTable '(("col1" "col2" "col3") (0 2 3) (4 5 6)))


(define (parse-column-metadata2 p)
 (for/list ([i p])
      (let ((type (car (list-ref i 3)))
            (colName (cadr i)))
        (val (string->symbol colName)
             (cond [(= type 1) integer?]
                   ; type 2 is a date and needs to be changed because a symblic date type wont be accepted by Rosette
                   [(= type 2) integer?]
                   [(= type 3) string?]
                   [(= type 4) boolean?]
                   [(= type 5) real?]
                   [#t string?])))))

(define (log req)
  (println req)
  (println "log"))

(define-values (req-dispatch req-url)
    (dispatch-rules
     [("synthesis" "synthesize") #:method (or "get" "post") synthesize]
     [("synthesis" "parse") #:method (or "get" "post") parse]
     [("synthesize") #:method (or "get" "post") synthesize]
     [("parse") #:method (or "get" "post") parse]
     [else log]))

(serve/servlet req-dispatch
               #:listen-ip #f
               #:servlet-regexp #rx".*" 
               #:launch-browser? #f)

(provide parse-column-metadata)
