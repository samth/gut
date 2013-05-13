;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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

#lang typed/racket

(provide:
  [parse-url (String -> (Either ParseError Url))]
  [parse-hier (Input-Port -> (values (Option String) String))]
  [parse-query (Input-Port -> (Either ParseError QParams))]
  [parse-fragment (Input-Port -> (Option String))]
  [parse-authority (String Scheme -> (Option Authority))])

(require 
 (only-in type/either
	  Either Left Right Left? Right?
	  left right)
 (only-in type/opt
	  opt-apply-orelse
	  opt-map)
 (only-in type/string
	  null-string?
	  default-string)
 (only-in "../types.rkt"
	  Uri Uri-scheme Scheme)
 (only-in "../urichar.rkt"	  
	  digit-char?)
 (only-in "../parse-util.rkt"
	  read-until)
 (only-in "../parse.rkt"
	  ParseError
	  parse-scheme)
 "urlchar.rkt"
 "types.rkt")

(: make-url (Scheme Authority Path QParams Fragment -> Uri))
(define (make-url scheme authority path query fragment)
  (Url scheme authority
       (default-string path "/")
       query
       (opt-map fragment (λ: ((s : String))
			     (if (null-string? s) #f s)))))

;; lex a character of value chtok
;; returns: #f if the next character is not a chtok
(: parse-char (Input-Port Char -> Boolean))
(define (parse-char ip chtok)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
        #f
        (if (eq? ch chtok)
            (begin
              (read-char ip)
              #t)
            #f))))

(: parse-authority-opaque (Input-Port -> (Option String)))
(define (parse-authority-opaque ip)
  (let ((op (open-output-string)))
    (read-until ip op (case ch
                        ((#\/ #\? #\#) #f)
                        (else #t)))
    (let ((auth (get-output-string op)))
      (if (eq? (string-length auth) "")
          #f
          auth))))

(: parse-path-abempty (Input-Port -> String))
(define (parse-path-abempty ip)
  (let ((op (open-output-string)))
    (let ((ch (peek-char ip)))
      (if (or (eof-object? ch)
              (eq? ch #\?)
              (eq? ch #\#))
          ""
          (if (not (eq? ch #\/))
              (error "A URI with an authority can only have an absolute path.")
              (begin (read-char ip)
                     (write-char ch op)
                     (let ((ch (peek-char ip)))
                       (if (eq? ch #\/)
                           (error "Absolute path must have a none empty segment.  i.e., // is illegal")
                           (read-until ip op (or (pchar? ch)
                                                 (eq? ch #\/))))))))
      (get-output-string op))))

(: parse-path-absolute (Input-Port -> String))
(define (parse-path-absolute ip)  
  (let ((op  (open-output-string)))    
    ;; first segment must not have a ':'
    (read-until ip op (and (not (eq? ch #\:))
                           (pchar? ch)))
    (read-until ip op (or (pchar? ch)
                          (eq? ch #\/)))                
    (get-output-string op)))

(: parse-path-rootless (Input-Port -> String))
(define (parse-path-rootless ip)  
  (let ((op (open-output-string)))
    (read-until ip op (or (pchar? ch)
                          (eq? ch #\/)))
    (get-output-string op)))

;; returns 2 values
;;  1) opaque authority string or #f
;;  2) path
(: parse-hier (Input-Port -> (values (Option String) String)))
(define (parse-hier ip)  
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
        (values #f "")
        (if (eq? ch #\/)
            (begin
              (read-char ip)
              (if (eq? (peek-char ip) #\/)
                  (begin
                    (read-char ip) ; xxxx:// has been read
                    (if (eq? (peek-char ip) #\/)                        
                        (values #f (parse-path-absolute ip))
                        (let ((authority (parse-authority-opaque ip)))
                          (let ((path-abempty (parse-path-abempty ip)))
                            (values authority path-abempty)))))
                  (values #f (parse-path-absolute ip))))
            (values #f (parse-path-rootless ip))))))

(: encode-char-out (Char Output-Port -> Void))
(define (encode-char-out ch outp)
  (write-char #\% outp)
  (display (string-upcase (number->string (char->integer ch) 16)) outp))

(: encode-out (Char Output-Port -> Output-Port))
(define (encode-out ch outp)  
  (if (unreserved-char? ch)
      (write-char ch outp)
      (encode-char-out ch outp))
  outp)

(: MISSING-QUERY-PARAM-NAME String)
(define MISSING-QUERY-PARAM-NAME "Query contains a '=' without a parameter name on the left hand side.")

(: parse-query (Input-Port -> (Either ParseError QParams)))
(define (parse-query ip)       

  (: end-of-query (Output-Port (Option String) QParams -> (Right QParams)))
  (define (end-of-query os name params)
    (let ((token (get-output-string os)))			
      (Right (if name 
		 (reverse (cons (QParam name token) params))
		 (if (null-string? token)
		     (reverse params)
		     (reverse (cons (QParam token "") params)))))))

  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
	(Right '())    
	(if (char=? ch #\?)      
	    (begin
	      (read-char ip) ;; burn ?
	      (let: loop : (Either ParseError QParams) 
		    ((ch : (U EOF Char) (read-char ip))
		     (os : Output-Port (open-output-string))
		     (name : (Option String) #f) 
		     (params : QParams '()))
		    (if (eof-object? ch)
			(end-of-query os name params)
			(cond
			 ((char=? ch #\#)
			  (end-of-query os name params))
			 ((char=? ch #\=)
			  (if name
			      (loop (read-char ip) (encode-out ch os) name params)
			      (let ((name (get-output-string os)))
				(if (null-string? name)
				    (Left (ParseError MISSING-QUERY-PARAM-NAME))
				    (loop (read-char ip)
					  (open-output-string)
					  name
					  params)))))
			 ((char=? ch #\&)
			  (if name
			      (loop (read-char ip)
				    (open-output-string)
				    #f
				    (cons (QParam name (get-output-string os)) params))
			      (loop (read-char ip)
				    (open-output-string)
				    #f
				    (cons (QParam (get-output-string os) "") params))))
			 (else 
			  (loop (read-char ip) (encode-out ch os) name params))))))
	    (Right '())))))

(: parse-fragment (Input-Port -> (Option String)))
(define (parse-fragment ip)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
	#f
	(begin 
	  (when (char=? ch #\#)
		(read-char ip))
	  (let loop ((ch (read-char ip)) (os (open-output-string)))    
	    (if (eof-object? ch)
		(let ((frag (get-output-string os)))
		  (if (null-string? frag) #f frag))
		(loop (read-char ip) (encode-out ch os))))))))

;; Parse out the port string.
;; Assumes leading ':' has been consumed.
(: parse-port (Input-Port -> (Option Natural)))
(define (parse-port ip)
  (let ((ch (read-char ip)))
    (if (eof-object? ch)  ;; no port
        #f
        (if (not (eq? ch #\:))
            (error "Host must be optionally followed by a port.  Something else found.")
            (let ((op  (open-output-string))
                  (ch  (peek-char ip)))
              (if (or (eof-object? ch)
                      (not (digit-char? ch)))
                  (error "Missing port number or extraneous characters where port number was expected.")
                  (let ((port (begin (read-until ip op (digit-char? ch))                                                 
                                     (get-output-string op))))
                    (let ((port (string->number port)))
                      (if (and (exact-integer? port)
                               (>= port 0))
                          port
                          (error "Invalid port (not a number?)"))))))))))

;;; Parse the host and optional port from a given string
;;; returns: (values host port)
(: parse-host (Input-Port -> (Option String)))
(define (parse-host ip)
  (let ((op  (open-output-string)))
    (if (eof-object? (peek-char ip))
        (error "URI missing required host.")        
        (begin
          (read-until ip op (or (unreserved-char? ch)
                                (pct-encoded-char? ch)
                                (sub-delim-char? ch)))
          (let ((host (get-output-string op)))
            (if (> (string-length host) 0)
                host
                #f))))))

(: parse-user (Input-Port -> (Option String)))
(define (parse-user ip)
  (let ((op  (open-output-string)))
    (read-until ip op (or (unreserved-char? ch)
                          (pct-encoded-char? ch)
                          (sub-delim-char? ch)
                          (eq? ch #\:)))                
    (if (not (eq? (read-char ip) #\@))
        #f
        (get-output-string op))))

(: scheme-default-port (Scheme -> (Option Natural)))
(define (scheme-default-port scheme)
  (if (symbol? scheme)
      (case scheme
	((HTTP) 80)
	((HTTPS) 443)
	(else #f))
      #f))

(: parse-authority (String Scheme -> (Option Authority)))
(define (parse-authority auth-str scheme)
  (if (not (string? auth-str))
      #f
      (let ((ip (open-input-string auth-str)))
        (let ((user (parse-user ip)))
          (let ((ip (if user
                        ip
                        (open-input-string auth-str)))) ;; restart parse
            (let ((host (parse-host ip))
                  (port (let ((p (parse-port ip)))                          
                          (if (and p (>= p 0)) p (scheme-default-port scheme)))))
              (if host
                  (Authority user host port)
                  #f)))))))

(: parse-url (String -> (Either ParseError Url)))
(define (parse-url uri-str)
  (let ((ip (open-input-string uri-str)))
    (let: ((scheme : (Either ParseError Scheme) (parse-scheme ip)))
      (if (Left? scheme)
	  scheme
	  (let-values (((authority path) (parse-hier ip)))
	    (let* ((auth (if (string? authority)
			     (parse-authority authority (right scheme))
			     #f))
		   (query (parse-query ip))
		   (fragment (parse-fragment ip)))
	      (if (Left? query)
		  query
		  (if auth
		      (Right (Url (right scheme) auth path (right query) fragment))
		      (Left (ParseError "Invalid scheme. Missing ':'?"))))))))))

 ;; [uri->string (Uri -> String)]
 ;; [parse-http-path (String -> (Listof (Option String)))]
 ;; [uri->start-line-path-string (Uri -> String)]
 ;; [http-path-path ((Listof String) -> String)]
 ;; [http-path-query ((Listof String) -> String)]
 ;; [http-path-fragment ((Listof String) -> String)]
 ;; [extend-path (Uri String -> Uri)]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to parse a HTTP request start line path.				 ;;
;; Given start-line "GET /a/b/c/d.txt?x=2#one" 				 ;;
;; (parse-http-start-line-path start-line) => ("a/b/c/d.txt"  "x=2" "one")	 ;;
;; The other routines are just sugar to extract out the path, query or fragment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (: http-path-path ((Listof String) -> String))
;; (define (http-path-path slpath)  
;;   (car slpath))

;; (: http-path-query ((Listof String) -> String))
;; (define (http-path-query slpath)  
;;   (cadr slpath))

;; (: http-path-fragment ((Listof String) -> String))
;; (define (http-path-fragment slpath)
;;   (caddr slpath))

;; (: parse-http-path (String -> (Listof (Option String))))
;; (define (parse-http-path path-str)
;;   (let ((ip (open-input-string path-str)))
;;     (let-values (((auth path) (parse-hier ip)))
;;       (let ((query (parse-query-or-fragment ip #\?)))
;;         (let ((fragment (parse-query-or-fragment ip #\#)))
;;           (list path query fragment))))))

;; (: extend-path (Url String -> Ur))
;; (define (extend-path url relative-path)
;;   (struct-copy Url url [path (path->string (build-path (Url-path uri)
;;                                                        (string->path relative-path)))]))


