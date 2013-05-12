#lang typed/racket/base

#| Sorta kinda compliant GUID/UUID generator.  
A Version 1 generator based off of RFC4122 
This module is Racket thread safe. |#

(provide:
 [uuid (-> String)])

(require/typed racket/format 
               [~a (Any
                    [#:align (U 'left 'center 'right)]
                    [#:left-pad-string String]
                    [#:width Integer] -> String)])

(require 
 system/interface)

(define default-interface "eth0")

(: node String)
(define node (interface-mac-address default-interface))

;; Determine the Timestamp segment of the GUID

;; timestamp-version is 8 hex octets
;; or 16 nibbles, 16 hex chars
;; high nibble is the version, which is 1.
(: timestamp-version (-> String))
(define (timestamp-version)
  (let ((ts (+ (* (inexact->exact (* (current-inexact-milliseconds) 1000)) 1000)
               (random 1000)))) ;; above give micro secs, simulate nanos, which is silly I know.
    (string-append "1"
                   (~a (format "~x" ts) 
                       #:align 'right
                       #:width 15
                       #:left-pad-string "0"))))

;; Determine the clock sequence part of the GUID
;; The clock sequence is a rolling 14 bit number starting
;; from a random value.

(: SEQ-MAX Integer)
(define SEQ-MAX (expt 2 14))

;; sets the most sig two bits in the top 6 bits of the 
;; clock sequence to "10"
(: VARIANT Integer)
(define VARIANT 32768)

(define clock-sequence-semaphore (make-semaphore 1))
(define clock-sequence (random SEQ-MAX))

(define (next-clock-sequence)  
  (semaphore-wait clock-sequence-semaphore)  
  (let ((cs (add1 clock-sequence)))
    (if (>= cs SEQ-MAX)
        (set! clock-sequence 0)
        (set! clock-sequence cs))
    (semaphore-post clock-sequence-semaphore)
    clock-sequence))

(: clock-sequence-and-variant (-> String))
(define (clock-sequence-and-variant)
  (format "~x" (+ VARIANT (next-clock-sequence)))) ;; set most sig two bits to '10'.

;; Build the guid segments from the above major segments

(: time-low (String -> String))
(define (time-low timestamp-version)
  (substring timestamp-version 8 16))

(: time-mid (String -> String))
(define (time-mid timestamp-version)                  
  (substring timestamp-version 4 8))

(: time-high (String -> String))
(define (time-high timestamp-version)
  (substring timestamp-version 0 4))

(: uuid (-> String))
(define (uuid)
  (let ((ts-v (timestamp-version))
        (cs-v (clock-sequence-and-variant)))
    (format "~a-~a-~a-~a-~a" 
            (time-low ts-v)
            (time-mid ts-v)
            (time-high ts-v)
            cs-v
            node)))
