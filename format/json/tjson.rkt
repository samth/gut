#lang typed/racket/base

(provide
 JsObject-empty
 jsobject jsobject-opt json->jsobject
 jsobject-add-attribute
 jsobject-remove-attribute
 Json JsNull JsObject JsList
 json->string string->json write-json read-json)

(define js-null 'JsNull)
(define-type JsNull 'JsNull)
(define-predicate JsNull? JsNull)
(define-type Json (Rec Json (U String Boolean JsNull Number (Listof Json) (HashTable Symbol Json))))
(define-type JsObject (HashTable Symbol Json))
(define-type JsList (Listof Json))

(: JsObject-empty JsObject)
(define JsObject-empty (make-hash))

(: write-json (Json Output-Port -> Void))
(define (write-json json port)
  
  (: write-object ((HashTable Symbol Json) -> Void))
  (define (write-object json)
    (display "{" port)
    (for ([(key value) json]
          [i (in-naturals)])
      (when (> i 0)
        (display ", " port))
      (fprintf port "\"~a\"" key)
      (display ": " port)
      (write-json value port))
    (display "}" port))
  
  (: write-list (JsList -> Void))
  (define (write-list json)
    (display "[" port)
    (for ([(value i) (in-indexed json)])
      (when (> i 0)
        (display ", " port))
      (write-json value port))
    (display "]" port))
  
  (cond
   [(hash? json) (write-object (cast json JsObject))]
   [(list? json) (write-list json)]   
   [(or (string? json) (and (number? json) (or (integer? json) (inexact? json))))
    (write json port)]
   [(boolean? json) (write (if json 'true 'false) port)]
   [(JsNull? json) (write 'null port)]
   [else (error 'json "bad json value: ~v" json)])) ;; Can't happen :)

(: read-json (Input-Port -> Json))
(define (read-json port)
  (case (peek-char port)
    [(#\{) (read/hash port)]
    [(#\[) (read/list port)]
    [(#\") (read/string port)]
    [(#\t) (read/true port)]
    [(#\f) (read/false port)]
    [(#\n) (read/null port)]
    [else (read/number port)]))

(: expect ((U EOF Char) (Listof Char) -> Char))
(define (expect ch expected)
  (if (eof-object? ch)
      (error 'read "unexpected EOF when expecting ~v" expected)
      (begin
        (unless (memq ch expected)
          (error 'read "expected: ~v, got: ~a" expected ch))
        ch)))

(: expect-string (Input-Port (Listof Char) -> String))
(define (expect-string port expected)
  (list->string (for/list ([ch expected])
                  (let ((c (read-char port)))
                    (if (eof-object? c)
                        (error 'read-string "expected ~v but unexpecte EOF" expected)
                        (expect c (list ch)))))))

(: skip-whitespace (Input-Port -> Void))
(define (skip-whitespace port)
  (let ([ch (peek-char port)])
    (when (and (char? ch)
               (char-whitespace? ch))
      (read-char port)
      (skip-whitespace port))))

(: read-until (All (a) Input-Port (Input-Port -> a) (Input-Port -> Boolean) -> (Listof a)))
(define (read-until port reader done?)
  (if (done? port)
      '()
      (let: loop : (Listof a) ((accum : (Listof a) '()))
	    (if (done? port)
		(reverse accum)
		(loop (cons (reader port) accum))))))

(: read/hash (Input-Port -> JsObject))
(define (read/hash port)
  
  (: read-key-json (Input-Port -> (Pair Symbol Json)))
  (define (read-key-json port)
    (skip-whitespace port)
    (let ([key (read/string port)])
      (skip-whitespace port)
      (expect (read-char port) '(#\:))
      (skip-whitespace port)
      (let ([value (read-json port)])
        (skip-whitespace port)
        (expect (peek-char port) '(#\, #\}))
        (when (eq? (peek-char port) #\,)
          (read-char port))
        (cons (string->symbol key) value))))
  
  (expect (read-char port) '(#\{))
  (skip-whitespace port)
  
  (let ((kvs (read-until port
                         read-key-json
                         (lambda: ((port : Input-Port))
                           (eq? (peek-char port) #\})))))
    (skip-whitespace port)
    (expect (read-char port) '(#\}))
    (make-hasheq kvs)))

(: read/list (Input-Port -> JsList))
(define (read/list port)
					;(display "read/list")
  (expect (read-char port) '(#\[))
  (let: ((json : JsList  
               (for/list: : JsList 
			  ([value : Json
				  (read-until port
					      (lambda: ((port : Input-Port))
						(skip-whitespace port)
						(begin0 (read-json port)
						  (skip-whitespace port)
						  (let ((ch (peek-char port)))
						    (expect ch '(#\, #\]))
						    (when (and (char? ch)
							       (char=? ch #\,))
						      (read-char port)))))
					      (lambda: ((port : Input-Port))
						(skip-whitespace port)
						(let ((ch (peek-char port)))
						  (if (char? ch)
						      (char=? ch #\])
						      #f))))])
			  value)))
    (expect (read-char port) '(#\]))
    json))

(: read/string (Input-Port -> String))
(define (read/string port)
  (expect (read-char port) '(#\"))
  (begin0 (list->string
           (for/list ([ch ((inst read-until Char) 
                           port
                           (lambda (port)
                             (let ([ch (read-char port)])
                               (cond 
				((eof-object? ch)
				 (error 'read "unexpected EOF"))
				((char? ch)
				 (if (eq? ch #\\)
				     (let ([esc (read-char port)])
				       (if (eof-object? esc)
					   (error 'read "unexpected EOF")
					   (case esc
					     [(#\b) #\backspace]
					     [(#\n) #\newline]
					     [(#\r) #\return]
					     [(#\f) #\page]
					     [(#\t) #\tab]
					     [(#\\) #\\]
					     [(#\") #\"]
					     [(#\/) #\/]
					     [(#\u) (unescape (read-string 4 port))]
					     [else esc])))
				     ch)))))
                           (lambda (port)
                             (eq? (peek-char port) #\")))])
             ch))
    (expect (read-char port) '(#\"))))

(: unescape ((U EOF String) -> Char))
(define (unescape str)
  (if (eof-object? str)
      (error 'read "unexpected EOF while reading \\u encoding")
      (begin
        (unless (regexp-match #px"[a-fA-F0-9]{4}" str)
          (error 'read "bad unicode escape sequence: \"\\u~a\"" str))
        (let ((n (string->number str 16)))
          (if (exact-integer? n)
              (integer->char n)
              (error 'read "bad unicode escape sequence: \"\\u~a\"" str))))))

(: true-seq (Listof Char))
(define true-seq (string->list "true"))

(: read/true (Input-Port -> True))
(define (read/true port)
  (expect-string port true-seq)
  #t)

(: false-seq (Listof Char))
(define false-seq (string->list "false"))

(: read/false (Input-Port -> False))
(define (read/false port)
  (expect-string port false-seq)
  #f)

(: null-seq (Listof Char))
(define null-seq (string->list "null"))

(: read/null (Input-Port -> JsNull))
(define (read/null port)
  (expect-string port null-seq)
  js-null)

(: read/digits (Input-Port -> (Listof Char)))
(define (read/digits port)
  (let: ([digits : (Listof Char) 
                 (for/list ([digit ((inst read-until Char)
                                    port
                                    (lambda (port) 
                                      (let ((ch (read-char port)))
                                        (if (eof-object? ch)
                                            (error 'read "unexpected eof while reading digits")
                                            ch)))
                                    (lambda (port)
                                      (let ((ch (peek-char port)))
                                        (cond 
					 ((eof-object? ch) #t)
					 ((char? ch)  (not (char-numeric? ch)))
					 (else #f)))))])
                   digit)])
    (when (and (null? digits) (eof-object? (peek-char port)))
      (error 'read "unexpected EOF"))
    (when (null? digits)
      (error 'read "expected: digits, got: ~a" (peek-char port)))
    digits))


(: exponent-seq (Listof Char))
(define exponent-seq (list #\e #\E))

(: read/exponent (Input-Port -> (Listof Char)))
(define (read/exponent port)
  (expect (read-char port) exponent-seq)
  (let ([sign (case (peek-char port)
                [(#\- #\+) (let ((ch (read-char port)))
                             (if (eof-object? ch)
                                 (error 'read "unexpected file while reading exponent")
                                 (list ch)))]
                [else '()])])
    (append sign (read/digits port))))

(: read/number (Input-Port -> Number))
(define (read/number port)
  (let* ([sign (if (eq? (peek-char port) #\-) (begin (read-char port)'(#\-)) '())]
         [digits (read/digits port)]
         [frac (if (eq? (peek-char port) #\.) 
                   (begin
                     (read-char port)
                     (cons #\. (read/digits port)))
                   '())]
         [exp (if (memq (peek-char port) exponent-seq) (read/exponent port) '())]
         [nstr (append sign digits frac exp)])
    (let ((n (string->number (list->string nstr))))
      (if (number? n)
          n
          (error 'read "failure parsing number ~a" nstr)))))

(: json->string (Json -> String))
(define (json->string x)
  (let ([out (open-output-string)])
    (write-json x out)
    (get-output-string out)))

(: string->json (String -> Json))
(define (string->json s)
  (let ([in (open-input-string s)])
    (read-json in)))


;; Helpers to make json

(: json->jsobject (Json -> JsObject))
(define (json->jsobject json)
  (if (hash? json)
      (cast json JsObject)
      (error 'Json->JsObject "Json is not a JsObject: ~s" json)))

(: jsobject ((Listof (Pair Symbol Json)) -> JsObject))
(define (jsobject attrs)
  (make-hasheq attrs))

;; Careful will filter out boolean values, which are #f
;; string values which are ""
;; object values which are empty
;; list values are emmpty.
(: jsobject-opt ((Listof (Pair Symbol Json)) -> JsObject))
(define (jsobject-opt attrs-opt)
  (let: ((attrs : (Listof (Pair Symbol Json))
                (filter (λ: ((attr : (Pair Symbol Json)))
			    (let ((value (cdr attr)))
			      (cond
			       [(boolean? value)  value]
			       [(string? value) (not (string=? value ""))]
			       [(hash? value) (> (hash-count (cast value JsObject)) 0)]
			       [(list? value) (pair? value)])))
                        attrs-opt)))
    (apply jsobject (list attrs))))

(: jsobject-add-attribute (JsObject Symbol JsObject -> Void))
(define (jsobject-add-attribute obj key value)
  (hash-set! obj key value))

(: jsobject-remove-attribute (JsObject Symbol -> Void))
(define (jsobject-remove-attribute obj key)
  (hash-remove! obj key))

(: attribute (JsObject Symbol JsObject -> Void))
(define (attribute obj key value)
  (hash-set! obj key value))
