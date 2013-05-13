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

(provide:
 [digit-char? (Char -> Boolean)] 
 [alphabet-char? (Char -> Boolean)])

(: digit-char? (Char -> Boolean))
(define digit-char?
  (lambda (ch)
    (and
     (char>=? ch #\0)
     (char<=? ch #\9))))

(: alphabet-char? (Char -> Boolean))
(define alphabet-char?
  (lambda (ch)
    (and
     (char-ci>=? ch #\a)
     (char-ci<=? ch #\z))))
