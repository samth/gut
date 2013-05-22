;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010,2011  Raymond Paul Racine
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

#| HTTP Cookies |#

#lang typed/racket/base

(provide make-cookie
	 parse-cookie)

(require/typed 
 racket/base
 (opaque Date date?)
 (seconds->date (Integer -> Date)))

(require/typed
 racket/date
 (current-date (-> Date))
 (date-display-format (Parameterof Symbol))
 (date->string (Date -> String)))

(require
 (only-in "heading.rkt"
	  COOKIE SET-COOKIE)
 (only-in "header.rkt"
	  make-header Headers Header
	  get-header-value))


;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the cookie header from a headers
;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: get-cookie-header (Headers -> (Option String)))
(define (get-cookie-header headers)
  (get-header-value COOKIE headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make a cookie header ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(: cookie-header (String -> Header))
(define (cookie-header cookie)
  (make-header SET-COOKIE cookie))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a HTTP Header Cookie				            ;;
;; (make-cookie "ray.com" "/a/b" "bread" "rye" 500) -> 	            ;;
;; "bread=rye; Expires=Sat, 21 May 2011; Path=/a/b; Domain=ray.com" ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: make-cookie (String String String String Integer -> String))
(define (make-cookie domain path name value expire-secs)
  (let ((duration (+ (current-seconds) expire-secs)))
    (parameterize ((date-display-format 'rfc2822))
      (let ((expire-date (date->string (seconds->date duration))))
	(string-append name "=" value "; "
		       "Expires=" expire-date "; "
		       "Path="    path "; "
		       "Domain="  domain)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse a cookie string into an alist of key value pairs		   ;;
;; (parse-cookie (make-cookie "ray.com" "/a/b" "bread" "rye" 500)) ->	   ;;
;; '(("Path" . "/a/b") ("Expires" . "Sat, 21 May 2011") ("bread" . "rye")) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: parse-cookie (String -> (Listof (Pairof String String))))
(define (parse-cookie cookie)
  (let ((is (open-input-string cookie))
	(os (open-output-string)))
    (let: loop : (Listof (Pair String String)) 
	  ((avs : (Listof (Pair String String)) '()) 
	   (ws : Boolean #f) 
	   (attr : (Option String) #f))
	  (let ((ch (read-char is)))
	    (cond 
	     ((eof-object? ch)
	      (if (and (string? attr)
		       (not (string=? attr "")))
		  (cons (cons attr (get-output-string os)) avs)
		  avs))
	     ((and (not ws) (char=? ch #\space))
	      (loop avs ws attr))
	     (else
	      (if attr
		  (if (char=? ch #\;)
		      (let ((value (bytes->string/utf-8 (get-output-bytes os #T))))
			(loop (cons (cons attr value) avs) #f #f))
		      (let ((ws (if (char=? ch #\")
				    (not ws)
				    #t)))
			(write-char ch os)
			(loop avs ws attr)))
		  (if (char=? ch #\=)
		      (let ((attr (bytes->string/utf-8 (get-output-bytes os #t))))
			(loop avs ws attr))
		      (begin
			(write-char ch os)
			(loop avs ws attr))))))))))
