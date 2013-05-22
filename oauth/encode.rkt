#lang typed/racket/base

(provide:
 [encode (String -> String)])

(require 
 (only-in net/uri/urichar
	  alphabet-char?
	  digit-char?))

;; Do not encode these symbol chars.
(: clear-sym-char? (Char -> Boolean))
(define (clear-sym-char? ch)
  (case ch
    ((#\- #\. #\_ #\~) #t)
    (else #f)))

(: clear-char? (Char -> Boolean))
(define (clear-char? ch)
  (or (alphabet-char? ch)
      (digit-char? ch)
      (clear-sym-char? ch)))

(: encode-char-out (Char Output-Port -> Void))
(define (encode-char-out ch outp)
  (write-char #\% outp)
  (display (string-upcase (number->string (char->integer ch) 16)) outp))

(: encode-out (Char Output-Port -> Output-Port))
(define (encode-out ch outp)  
  (if (clear-char? ch)
      (write-char ch outp)
      (encode-char-out ch outp))
  outp)

(: encode (String -> String))
(define (encode s)
  (let ((is (open-input-string s)))
    (let loop ((ch (read-char is)) (os (open-output-string)))
      (if (eof-object? ch)
	  (get-output-string os)
	  (loop (read-char is) (encode-out ch os))))))
