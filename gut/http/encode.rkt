#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2012  Raymond Paul Racine
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

(provide
 url-encode-string
 url-decode-string
 url-decode-from-input-port)

(require
 (only-in net/uri/url/urlchar
	  hex-char?
	  encode-char
	  unreserved-char?))

;; Encode the given string
;; space-as-plus boolean denotes if spaces should be encoded with '+' or %20.
(: url-encode-string (String Boolean -> String))
(define (url-encode-string str space-as-plus)
  (let ((is (open-input-string str))
	(os (open-output-string)))
    (let: loop : String ((ch : (U Char EOF) (read-char is)))
	  (cond
	   ((eof-object? ch) (get-output-string os))
	   ((unreserved-char? ch) (begin (write-char ch os)
					 (loop (read-char is))))
	   (else (if (and space-as-plus
			  (char=? ch #\space))
		     (begin (write-char #\+ os)
			    (loop (read-char is)))
		     (begin (write-string (encode-char ch) os)
			    (loop (read-char is)))))))))

(: url-decode-from-input-port (Input-Port (Option Char) Boolean -> String))
(define (url-decode-from-input-port ip delim decode-plus?)
  (let ((op (open-output-string)))
    (let: loop : String ((ch : (U Char EOF) (read-char ip)))
	  (if (or (eof-object? ch)
		  (and delim (char=? ch delim)))
	      (get-output-string op)
	      (if (char=? #\% ch)
		  (let ((ch1 (read-char ip)))
		    (if (and (not (eof-object? ch1))
			     (hex-char? ch1))
			(let ((ch2 (read-char ip)))
			  (if (and (not (eof-object? ch2))
				   (hex-char? ch2))
			      (begin ;; use (let ((buff (make-string 2))) ???
				(write-char (integer->char (assert (string->number (list->string (list ch1 ch2)) 16) 
								   exact-integer?))
					    op)
				(loop (read-char ip)))
			      (begin              ;; got %d? so write '%' and digit and carryon
				(write-char #\% op)
				(write-char ch1 op)
				(unless (eof-object? ch2)
				  (write-char ch2 op))
				(loop (read-char ip)))))
			(begin                   ;; got %? so write them and carryon
			  (write-char #\% op)
			  (unless (eof-object? ch1)
			    (write-char ch1 op))
			  (loop (read-char ip)))))
		  (begin
		    (unless (eof-object? ch)
		      (if (and decode-plus?
			       (char=? ch #\+))
			  (write-char #\space op)
			  (write-char ch op)))
		    (loop (read-char ip))))))))


;; Read from the input port until eof or delim char
;; URL decode as well.
;; if delim = #f then process till eof
(: url-decode-string (String (Option Char) Boolean -> String))
(define (url-decode-string str delim decode-plus?)
  (url-decode-from-input-port (open-input-string str) delim decode-plus?))
