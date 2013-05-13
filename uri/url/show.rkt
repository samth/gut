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

#| String representation of a Url |#

(provide:
 [url->string (Url -> String)])

(require 
 (only-in type/string
	  null-string?)
 (only-in type/opt
	  opt-apply-orelse)
 (only-in text/util
	  weave-string-separator)
 (only-in "../types.rkt"
	  Uri-scheme)
 (only-in "../show.rkt"
	  scheme->string)
 (only-in "util.rkt"
	  maybe)
 "types.rkt")

(: qparams->string (QParams -> (Option String)))
(define (qparams->string qparams)
  (if (null? qparams)
      #f
      (weave-string-separator "&" (map (λ: ((qparam : QParam))
					   (let ((val (QParam-value qparam)))
					     (if (null-string? val)
						 (QParam-name qparam)
						 (string-append (QParam-name qparam)
								"="
								(QParam-value qparam)))))
				       qparams))))

(: url->string (Url -> String))
(define (url->string url)
  (string-append   
   (scheme->string (Uri-scheme url))
   ":"
   (let ((auth (authority->string (Url-authority url))))
     (if auth
         (string-append "//" auth)
         ""))
   (Url-path url)
   (maybe (qparams->string (Url-query url)) "?")
   (maybe (Url-fragment url) "#")))

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
