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

(provide
 (struct-out ParseError))

(provide:
 [is-standard-scheme? (Scheme -> Boolean)]
 [parse-scheme (Input-Port -> (Either ParseError Scheme))])

(require
 (only-in type/string
	  null-string?)
 (only-in type/either
	  Either Left Right)
 (only-in "types.rkt"
	  Scheme)
 (only-in "urichar.rkt"
	  digit-char?
	  alphabet-char?)
 (only-in "parse-util.rkt"
	  read-until))

(struct: ParseError ([msg : String]) #:transparent)

(: is-standard-scheme? (Scheme -> Boolean))
(define (is-standard-scheme? scheme)
       (symbol? scheme))

(: standard-scheme-symbol (String -> Scheme))
(define (standard-scheme-symbol str)
  (let* ((str (string-downcase str))
	 (is? (λ: ((s : String))
		  (string=? s str))))
    (cond
     ((is? "http") 'HTTP)
     ((is? "https") 'HTTPS)
     ((is? "ftp")  'FTP)
     ((is? "file") 'FILE)
     (else str))))

(: scheme-start-ch? (Char -> Boolean))
(define scheme-start-ch? alphabet-char?)

(: scheme-tail-ch? (Char -> Boolean))
(define scheme-tail-ch?
  (lambda (ch)
    (or
     (scheme-start-ch? ch)
     (digit-char? ch)
     (case ch
       ((#\+ #\- #\.) #t)
       (else #f)))))

(: parse-scheme (Input-Port -> (Either ParseError Scheme)))
(define (parse-scheme ip)

  (define (bad-start-char)
    (Left (ParseError "Url scheme must start with alphabetic char.")))  
  
  (let ((start-ch (peek-char ip)))
    (if (eof-object? start-ch)
	(bad-start-char)
	(if (not (scheme-start-ch? start-ch))
	    (bad-start-char)
	    (let loop ((ch (read-char ip)) 
		       (op (open-output-string)))
	      (if (eof-object? ch)
		  (Left (ParseError "Missing ':' in Url."))
		  (if (char=? ch #\:)
		      (let ((scheme (get-output-string op)))
			(if (null-string? scheme)
			    (Left (ParseError "Url is missing required scheme."))
			    (Right (standard-scheme-symbol scheme))))
		      (if (scheme-tail-ch? ch)
			  (begin
			    (write-char ch op)
			    (loop (read-char ip) op))
			  (Left (ParseError (format "Invalid char in scheme: ~a" ch)))))))))))
