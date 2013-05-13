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

(provide
 read-until)

(provide:
 [read-valid (Input-Port (Char -> Boolean) Output-Port -> Integer)])

(require 
 (only-in "urichar.rkt"
	  digit-char?
	  alphabet-char?))

(define-syntax (read-until stx)
  (syntax-case stx ()
    [(_ ip op until-block)
     (with-syntax ([ch (datum->syntax stx 'ch )])
       #'(read-valid ip (λ: ((ch : Char)) until-block) op))]))

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
