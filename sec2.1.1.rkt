#lang racket

; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (negative? d)   ; might be more generic than (> d 0)
        (cons (/ (* n -1) g) (/ (* d -1)))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
