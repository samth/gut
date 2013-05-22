;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide Param Params)

(provide:

 ;; Param
 [param (String String -> Param)]
 [param-key (Param -> String)]
 [param-val (Param -> String)] 
 [param->noencode-string (Param -> String)]
 [param-keyval (Param -> (Values String String))]
 [encode-param (Param Boolean -> Param)]
 [param->query-kv (Param -> String)]
 [param->encoded-query-kv (case-> (Param -> String)
				  (Param Boolean -> String))]
 [encode-param-string (case-> (String Boolean -> String)
			      (String -> String))]

 ;; Params
 [make-params (Param * -> Params)]
 [parse-params (String -> Params)] ;;encode-param-string 
 [empty-params (-> Params)]
 [add-param (Param Params -> Params)] 
 [params->query (Params -> String)])

(require 
 (only-in typed/srfi/14
	  Char-Set
	  string->char-set
	  char-set-complement)
 (only-in type/text
	  weave-string-separator)
 (only-in net/uri/url/urlchar
	  encode-char
	  unsafe-char?))

(require/typed 
 srfi/13
 (string-tokenize (String Char-Set -> (Listof String))))

(define-type Param (Pair String String))

(define-type Params (Listof Param))

(define-predicate Param? Param)

(: make-params (Param * -> Params))
(define make-params
  (λ ps ps))

(: empty-params (-> Params))
(define (empty-params)
  '())

(: add-param (Param Params -> Params))
(define (add-param p ps)
  (cons p ps))

(: param (String String -> Param))
(define param (inst cons String String))

(: param-key (Param -> String))
(define param-key car)

(: param-val (Param -> String))
(define param-val cdr)

(: param-keyval (Param -> (Values String String)))
(define (param-keyval p)
  (values (car p) (cdr p)))

(: param->noencode-string (Param -> String))
(define (param->noencode-string param)
  (let-values (((k v) (param-keyval param)))
    (string-append k "=" v)))

(: param-reserved-char? (Char -> Boolean))
(define (param-reserved-char? ch)
  (case ch
    ((#\& #\=) #t)
    (else #f)))

(: encode-param-string (case-> (String Boolean -> String)
			       (String -> String)))
(define (encode-param-string str [space-as-plus #f])
  (let ((op (open-output-string))
	(ip (open-input-string str)))
    (let loop ((ch (read-char ip)))
      (cond 
       ((eof-object? ch) (get-output-string op))
       ((char=? ch #\space) 
	(if space-as-plus	   
	    (write-char #\+ op)
	    (write-string "%20" op))
	(loop (read-char ip)))
       ((or (unsafe-char? ch)
	    (param-reserved-char? ch))
	(write-string (encode-char ch) op)
	;;(write-char ch op)
	(loop (read-char ip)))
       (else
	(write-char ch op)
	(loop (read-char ip)))))))

;; (if (eof-object? ch)
;; 	 (get-output-string op)
;; 	 (begin
;; 	   (if (or (unsafe-char? ch)
;; 		 (param-reserved-char? ch)
;; 		 (char=? #\+ ch))
;; 	      (write-string (encode-char ch) op)
;; 	      (write-char ch op))
;; 	   (loop (read-char ip)))))))

(: encode-param (Param Boolean -> Param))
(define (encode-param param space-as-plus)
  (let ((key   (car param))
	(value (cdr param)))
    (cons (encode-param-string key space-as-plus)
	  (encode-param-string value space-as-plus))))

(: param->query-kv (Param -> String))
(define (param->query-kv p)
  (string-append (param-key p) "=" (param-val p)))

(: param->encoded-query-kv (case-> (Param -> String)
				 (Param Boolean -> String)))
(define (param->encoded-query-kv p [quote-value? #f])
  (string-append (encode-param-string (param-key p) #f)
		 "="
		 (let ((e-val (encode-param-string (param-val p) #f)))
		   (if quote-value?
		       (string-append "\"" e-val "\"")
		       e-val))))

(: params->query (Params -> String))
(define (params->query parms)
  (weave-string-separator "&" (map (λ: ((kv : (Pair String String)))
				       (string-append (encode-param-string (car kv) #f)
						      "=" 
						      (encode-param-string (cdr kv) #f)))
				   parms)))

(: param-delim-char-set Char-Set)
(define param-delim-char-set
  (char-set-complement (string->char-set "=&")))

(: parse-params (String -> Params))
(define (parse-params param-str)
  (let ((kvs (string-tokenize param-str param-delim-char-set)))
    (let: loop : Params ((kvs : (Listof String) kvs) (params : Params '()))
	  (if (null? kvs)
	      params
	      (let ((key (car kvs)))
		(if (null? (cdr kvs))
		    params ;; odd number of KVs which is wrong.  Return what we got.
		    (loop (cddr kvs) (cons (cons key (cadr kvs)) params))))))))
