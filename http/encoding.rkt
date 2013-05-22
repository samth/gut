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

#lang typed/racket/base

(provide
 parse-x-www-form-urlencoded)

(require
 (only-in srfi/14
	  char-set-complement
	  list->char-set)
 (only-in httpclient/encode
	  url-decode-from-input-port))

(: key-delim Char)
(define key-delim #\=)

(: value-delim Char)
(define value-delim #\&)

(: read-token (Input-Port Char -> String))
(define (read-token ip delim)
  (url-decode-from-input-port ip delim #t)) ;; decode + as space

;; parse port contents into an alist of (k . v) pairs
(: parse-x-www-form-urlencoded (Input-Port -> (Listof (Pair String String))))
(define (parse-x-www-form-urlencoded in-port)
  (let: loop : (Listof (Pair String String)) 
	((key : (Option String) #f) 
	 (kvs : (Listof (Pair String String))  '()))
	(if key
	    (loop #f (cons (cons key (read-token in-port value-delim)) kvs))
	    (let ((key (read-token in-port key-delim)))
	      (if (and (string? key)
		       (string=? key ""))
		  kvs
		  (loop key kvs))))))

