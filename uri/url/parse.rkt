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

#lang typed/racket/base

(require 
 "types.rkt")

(provide:
 [parse-uri (String -> (Option Uri))]
 [parse-authority (String String -> (Option Authority))]
 [uri->string (Uri -> String)]
 [parse-http-path (String -> (Listof (Option String)))]
 [uri->start-line-path-string (Uri -> String)]
 [http-path-path ((Listof String) -> String)]
 [http-path-query ((Listof String) -> String)]
 [http-path-fragment ((Listof String) -> String)]
 [extend-path (Uri String -> Uri)])

(: make-url (Scheme Authority Path QParams Fragment -> Uri))
(define (make-uri* scheme authority path query fragment)
  (Url scheme authority
       (opt-string path "/")
       (opt-string query)
       (opt-string fragment)))


;; Note NOT hygenic, captures the identifer `ch' 
;; which is required within the given `until-block' logic.
(define-syntax (read-until stx)
  (syntax-case stx ()
    [(_ ip op until-block)
     (with-syntax ([ch (datum->syntax stx 'ch )])
       #'(read-valid ip (λ: ((ch : Char)) until-block) op))]))


;; Prefix an optional string value.
;; An empty string if not defined.
(: maybe ((Option String) String -> String))
(define (maybe field prefix)
  (opt-apply-orelse field (λ: ((field : String))
                            (string-append prefix field)) ""))

;; Read chars while valid or eof-object?
;; Place valid chars in output string port
;; First invalid char is left on the input port
;; returns: number of valid chars read from input port.
(: read-valid (Input-Port (Char -> Boolean) Output-Port -> Integer))
(define (read-valid ip valid? op)
  (let loop ((ch (peek-char ip)) (cnt 0))
    (if (or (eof-object? ch)
            (not (valid? ch)))
        cnt
        (begin
          (write-char (assert (read-char ip) char?) op)
          (loop (peek-char ip) (add1 cnt))))))

(: uri->start-line-path-string (Uri -> String))
(define (uri->start-line-path-string uri)  
  (string-append
   (Uri-path uri)
   (maybe (Uri-query uri) "?")
   (maybe (Uri-fragment uri) "#")))

(: uri->string (Uri -> String))
(define (uri->string uri)
  (string-append
   (Uri-scheme uri)
   ":"
   (let ((auth (authority->string (Uri-authority uri))))
     (if auth
         (string-append "//" auth)
         ""))
   (Uri-path uri)
   (maybe (Uri-query uri) "?")
   (maybe (Uri-fragment uri) "#")))

(: authority->string ((Option Authority) -> (Option String)))
(define (authority->string authority)
  
  (: user-with-@ (Authority -> String))
  (define (user-with-@ authority)
    (let ((user (Authority-user authority)))
      (if (and user
               (> (string-length user) 0))
          (string-append user "@")
          "")))
  
  (: port-with-: (Authority -> String))
  (define (port-with-: authority)
    (let ((port (Authority-port authority)))
      (opt-apply-orelse port (λ: ((port : Natural)) (string-append ":" (number->string port)))
                        "")))
  
  (cond
    ((string? authority) authority)
    ((Authority? authority)
     (string-append (user-with-@ authority)
                    (Authority-host authority)
                    (port-with-: authority)))  
    (else #f)))

;; Two authorities are equal if they're record values are equal.
(: authority-equal? (Authority Authority -> Boolean))
(define (authority-equal? auth1 auth2)
  (and (equal? (Authority-user auth1)
               (Authority-user auth2))
       (equal? (Authority-host auth1)
               (Authority-host auth2))
       (eqv? (Authority-port auth1)
             (Authority-port auth2))))

;; (input-port?  output-port?) -> boolean?)
;; parse the "tail" of a scheme
;; i.e., the rest of the scheme string given that
;; the start char of the scheme was valid.
;; returns: # of chars read
(: parse-scheme-tail (Input-Port Output-Port -> Integer))
(define (parse-scheme-tail ip op)
  (read-until ip op (scheme-tail-ch? ch)))

(: parse-scheme (Input-Port -> (Option String)))
(define (parse-scheme ip)
  (let ((op (open-output-string)))
    (let ((ch (peek-char ip)))
      (if (eof-object? ch)
          #f
          (let ((ch (assert ch char?)))
            (if (not (scheme-start-ch? ch))
                #f
                (begin (read-char ip)
                       (write-char ch op)
                       (parse-scheme-tail ip op)
                       (get-output-string op))))))))

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

(: parse-query-or-fragment (Input-Port Char -> (Option String)))
(define (parse-query-or-fragment ip signal-char)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
        #f
        (if (eq? ch signal-char)
            (let ((op (open-output-string)))
              (read-char ip) ;; consume signal char
              (read-until ip op (or (pchar? ch)
                                    (eq? ch #\?)
                                    (eq? ch #\/)))
              (get-output-string op))
            #f))))

(: parse-uri (String -> (Option Uri)))
(define (parse-uri uri-str)
  (let ((ip (open-input-string uri-str)))
    (let ((scheme (parse-scheme ip)))
      (if (not scheme)
          #f
          (if (not (parse-char ip #\:))
              #f
              (let-values (((authority path) (parse-hier ip)))                
                (let ((auth (if (string? authority)
                                (parse-authority authority scheme)
                                #f)))
                  (let ((query (parse-query-or-fragment ip #\?)))
                    (let ((fragment (parse-query-or-fragment ip #\#)))
                      (Uri scheme auth path query fragment))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines to parse a HTTP request start line path.				 ;;
;; Given start-line "GET /a/b/c/d.txt?x=2#one" 				 ;;
;; (parse-http-start-line-path start-line) => ("a/b/c/d.txt"  "x=2" "one")	 ;;
;; The other routines are just sugar to extract out the path, query or fragment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: http-path-path ((Listof String) -> String))
(define (http-path-path slpath)  
  (car slpath))

(: http-path-query ((Listof String) -> String))
(define (http-path-query slpath)  
  (cadr slpath))

(: http-path-fragment ((Listof String) -> String))
(define (http-path-fragment slpath)
  (caddr slpath))

(: parse-http-path (String -> (Listof (Option String))))
(define (parse-http-path path-str)
  (let ((ip (open-input-string path-str)))
    (let-values (((auth path) (parse-hier ip)))
      (let ((query (parse-query-or-fragment ip #\?)))
        (let ((fragment (parse-query-or-fragment ip #\#)))
          (list path query fragment))))))

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

(: scheme-default-port (String -> (Option Natural)))
(define (scheme-default-port scheme)
  (cond
    ((string=? "http" scheme) 80)
    ((string=? "https" scheme) 443)
    (else #f)))

(: parse-authority (String String -> (Option Authority)))
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

(: extend-path (Uri String -> Uri))
(define (extend-path uri relative-path)
  (struct-copy Uri uri [path (path->string (build-path (Uri-path uri)
                                                       (string->path relative-path)))]))
