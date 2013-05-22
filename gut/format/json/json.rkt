#lang typed/racket/base

(provide
 json-obj-string
 json-obj-number
 json-obj-real
 json-obj-integer
 json->sjson
 JSon)

(require/typed
 (planet neil/json-parsing:1)
 (json->sjson (Input-Port -> JSon)))

(define-type JSon (Rec Json (U String Boolean Null Number (Listof Json) (HashTable Symbol Json))))

(: json-obj-string ((HashTable Symbol Any) Symbol -> String))
(define (json-obj-string obj symbol)  
  (let ((value ((inst hash-ref Symbol Any String) obj symbol (lambda () ""))))
    (if (string? value)
       value
       "")))    

(: json-obj-number ((HashTable Symbol Any) Symbol -> (Option Number)))
(define (json-obj-number obj symbol)
  (let ((value ((inst hash-ref Symbol Any Boolean) obj symbol (lambda () #f))))
    (if (number? value)
       value
       #f)))

(: json-obj-real ((HashTable Symbol Any) Symbol -> (Option Real)))
(define (json-obj-real obj symbol)
  (let ((num (json-obj-number obj symbol)))
    (if (real? num)
       num
       #f)))

(: json-obj-integer ((HashTable Symbol Any) Symbol -> (Option Integer)))
(define (json-obj-integer obj symbol)
  (let ((int (json-obj-integer obj symbol)))
    (if (exact-integer? int)
       int
       #f)))
