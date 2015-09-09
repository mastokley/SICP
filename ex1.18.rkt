#lang racket
; Exercise 1.18
;
; Using the results of exercises 1.16 and 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps.
;
; (This algorithm, which is sometimes known as the "Russian peasant method" of
; multiplication, is ancient. Examples of its use are found in the Rhind Papyus,
; one of the two oldest mathematical documents in existence, written about 1700
; B.C. (and copied from an even older document) by an Egyptian scribe named A'h-
; mose.)
;
; Result of exercise 1.16

(define (ssExpt b n)
  (define (ssExptIter base product currentPower remainderPower)
    (cond ((= remainderPower 1)
           product)
          ((= remainderPower 0)
           1)
          ((< (* currentPower 2)
              (+ currentPower remainderPower))
           (ssExptIter base
                       (* product product)
                       (* currentPower 2)
                       (- remainderPower currentPower)))
          (else (ssExptIter base
                            (* base product)
                            (+ currentPower 1)
                            (- remainderPower 1)))))
  (ssExptIter b b 1 n))

; Result of exercise 1.17

(define (fast-* x y)
  (define (even? x)
    (= 0 (remainder x 2)))
  (define (double x)
    (+ x x))
  (define (halve x)
    (cond ((even? x)
           (/ x 2))
          (else (display "error"))))
  (cond
    ((= y 0)
     0)
    ((= y 1)
     x)
    ((even? y)
     (double (fast-* x
                     (halve y))))
    (else (+ x (fast-* x (- y 1))))))

;

(define (russianPeasantMethod x y)
  (define (even? x)
    (= 0 (remainder x 2)))
  (define (double x)
    (+ x x))
  (define (halve x)
           (/ x 2))
  (define (iter x y runningTotal)
    (if (> x 1)
        (iter (truncate (halve x))
              (double y)
              (if (not (even? x))
                  (+ y runningTotal)
                  runningTotal))
        (if (not (even? x))
            (+ y runningTotal)
            runningTotal)))
  (iter x y 0))
            
; one iteration
; halve x, then round down to nearest integer
; double y
; if x is even, add y to runningTotal
; run iteration again with halved x