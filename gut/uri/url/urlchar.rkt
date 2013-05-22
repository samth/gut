;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
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

(provide 
 query-param-separator
 encode-char
 hex-char? pchar? pct-encoded-char?
 sub-delim-char? unreserved-char?
 unsafe-char?)

(require
 (only-in "../urichar.rkt"
	  alphabet-char? digit-char?))

(: query-param-separator String)
(define query-param-separator "&")

;; Amazon requires upcase letters in their signed URLs.
(: encode-char (Char -> String))
(define (encode-char ch)
    (string-append "%" (string-upcase (number->string (char->integer ch) 16))))

(: encode-char? (Char -> Boolean))
(define (encode-char? ch)
    (or (unsafe-char? ch)))

(: hex-char? (Char -> Boolean))
(define hex-char?
  (lambda (ch)
    (or
     (digit-char? ch)
     (case (char-downcase ch)
       ((#\a #\b #\c #\d #\e #\f) #t)
       (else #f)))))

(: unreserved-char? (Char -> Boolean))
(define unreserved-char?
  (lambda (ch)
    (or
     (alphabet-char? ch)
     (digit-char? ch)
     (case ch
       ((#\. #\_ #\~ #\\ #\-) #t)
       (else #f)))))

(: reserved? (Char -> Boolean))
(define (reserved? ch)
  (or (general-delim-char? ch)
     (sub-delim-char? ch)))

;; rtf1138
(: unsafe-char? (Char -> Boolean))
(define unsafe-char?
  (lambda (ch)
    (case ch
      ((#\{ #\} #\| #\\ #\^ #\~ #\[ #\] #\`)
       #t)
      (else #f))))

(: general-delim-char? (Char -> Boolean))
(define general-delim-char?
  (lambda (ch)
    (case ch
      ((#\: #\/ #\? #\# #\[ #\] #\@) #t)
      (else #f))))

(: sub-delim-char? (Char -> Boolean))
(define sub-delim-char?
  (lambda (ch)
    (case ch
      ((#\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=) #t)
      (else #f))))

(: pct-encoded-char? (Char -> Boolean))
(define pct-encoded-char?
  (lambda (ch)
    (or
     (eq? ch #\%)
     (hex-char? ch))))

(: encode-signal-char (Char -> Boolean))
(define (encode-signal-char ch)
  (char=? ch #\%))

(: pchar? (Char -> Boolean))
(define pchar?
  (lambda (ch)
    (or
     (unreserved-char? ch)
     (pct-encoded-char? ch)
     (sub-delim-char? ch)
     (case ch
       ((#\: #\@) #t)
       (else #f)))))
