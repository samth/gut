#lang typed/racket/base

(provide ACCEPT USER-AGENT DATE HOST 
	 CONTENT-TYPE CONTENT-LENGTH CONTENT-MD5
	 LOCATION COOKIE SET-COOKIE)

;; Standard Headers
(define ACCEPT         "Accept")
(define USER-AGENT     "User-Agent")
(define DATE           "Date")
(define HOST           "Host")
(define CONTENT-TYPE   "Content-Type")
(define CONTENT-LENGTH "Content-Length")
(define CONTENT-MD5    "Content-MD5")
(define LOCATION       "Location")
(define COOKIE         "Cookie")
(define SET-COOKIE     "Set-Cookie")
