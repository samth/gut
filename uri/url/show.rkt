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
 [url->path-query-fragment-string (Url -> String)]
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

(: url->path-query-fragment-string (Url -> String))
(define (url->path-query-fragment-string url)  
  (string-append
   (Url-path url)
   (maybe (qparams->string (Url-query url)) "?")
   (maybe (Url-fragment url) "#")))

