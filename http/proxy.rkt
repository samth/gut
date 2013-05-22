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

(provide set-http-proxy!
	 add-proxy-proc!
	 remove-proxy-proc!
	 http-proxy?
	 http-proxy-host
	 http-proxy-port)

(require
 (only-in net/uri/url/url
	  Url
	  Authority))

(: proxy-host (Option String))
(define proxy-host #f)
(: proxy-port (Option Integer))
(define proxy-port #f)

;; alistof (symbol? . authority? * uri? -> boolean?)
;; We pass that authority as its already been
;; parsed in http-invoke.
(: proxy-escape (Listof (Pairof Symbol (Authority Url -> Boolean))))
(define proxy-escape '())

(: http-proxy-host (-> (Option String)))
(define http-proxy-host
  (lambda ()
    proxy-host))

(: http-proxy-port (-> (Option Integer)))
(define http-proxy-port
  (lambda ()
    proxy-port))

(: set-http-proxy! (String Integer -> Void))
(define set-http-proxy!
  (lambda (host port)
    (set! proxy-host host)
    (set! proxy-port port)))

(: add-proxy-proc! (Symbol (Authority Url -> Boolean) -> Void))
(define (add-proxy-proc! symbol proc)
  (set! proxy-escape (cons (cons symbol proc) 
			   proxy-escape)))

(: remove-proxy-proc! (Symbol -> Void))
(define (remove-proxy-proc! symbol)
  (set! proxy-escape (filter (lambda: ((esc : (Pairof Symbol (Authority Url -> Boolean))))
				      (not (eq? symbol (car esc))))
			     proxy-escape)))

;; Determine if a http request should use the proxy or not.
(define http-proxy? 
  (lambda (authority uri)
    #f))
;; (lambda (authority uri)
;;   (let loop ((escapes proxy-escape))
;;     (if (null? escapes)
;; 	 #f
;; 	 (if ((cdr (car escapes)) authority uri)
;; 	    #t
;; 	    (loop (cdr escapes)))))))

