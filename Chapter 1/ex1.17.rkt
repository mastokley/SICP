#lang racket
; exponentiaton as repeated multiplication
; using successive squares
(define (fast-expt b n)
  (define (even? n)
    (= 0 (remainder n 2)))
  (define (square n)
    (* n n))
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; multiplication as repeated addition
; using repeated doubling/halving
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
; 5 x 4 => 5 + 5 + 5 + 5 => double (5) + double (5) => double (double 5)
; if y is even, double x and halve y
; if y is odd, add x to running total and decrement y
; 5 x 4 > (double 5) 2 > (double (double 5))
; 5 x 4 > 5*2 * 4/2 > 5
; 5 x 3 > double(5) + 5