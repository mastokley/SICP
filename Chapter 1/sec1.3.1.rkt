; 1.3 Formatting Abstractions with Higher-Order Procedures

#lang racket
(require "ex1.23.rkt") ; inlines (??) prime functions

(define (cube x)
  (* x x x))

(define (proto-sum-integers a b)
  (if (> a b)
    0
    (+ a (proto-sum-integers (+ a 1) b))))

(define (proto-sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (proto-sum-cubes (+ a 1) b))))

(define (proto-pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a
                 (+ a 2)))
       (proto-pi-sum (+ a 4) b))))

; procedure to generalize last three procedures
;
; takes as arguments 'term' and 'next', which are themselves procedures

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term
            (next a)
            next
            b))))

; you might use this procedure 'inc' as a value for the 'next' argument, above

(define (inc n)
  (+ n 1))

; here is the sum cubes procedure written in terms of sum procedure
;
; it makes use of 'cube' procedure defined above

(define (sum-cubes a b)
  (sum cube
       a
       inc
       b))

; here is the sum integers procedure written in terms of the sum procedure

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

; and finally the sum-pi procedure

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f 
          (+ a (/ dx 2.0)) 
          add-dx 
          b)
     dx))

; f is function
; a is lower bound
; b is upper bound
; n is number of terms

; exercise 1.29

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (inc k)
    (+ k 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0)
                  (= k n))
              1)
             ((even? k)
              2)
             (else 4))
       (y k)))
  (* (sum term
          0
          inc
          n)
     (/ h 3)))

; exercise 1.30

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (+ result 
               (term a)))))
  (iter a 0))

; exercise 1.31a
; write a procedure analogous to sum called product

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term
                (next a)
                next
                b))))

; show how to define factorial in terms of product

(define (factorial x)
  (product identity
           1
           inc
           x))

; also use product to compute approximations to pi using the formula
; pi / 4 = (2 * 4 * 4 * 6 * 6 * ...) / (3 * 3 * 5 * 5 * ...)
; => term = { 4n^2 / 4n^2 - 1}

;(define (square x)
;  (* x x))

(define (pi-approx n)
  (define (term i)
    (/ (* 4.0
          (square i))
       (- (* 4.0
             (square i))
          1)))
  (product term
           1
           inc
           n))

; exercise 1.31b
; write a product procedure that generates an iterative process

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (* result
               (term a)))))
  (iter a 1))

; Exercise 1.32
; show that sum and product are both special cases of a still more
; general notion called accumulate

(define (accumulate combiner
                    null-value
                    term
                    a
                    next
                    b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

; sum in terms of accumulate

(define (accumulateAsSum term a next b)
  (accumulate + 0 term a next b))

; product in terms of accumulate

(define (accumulateAsProduct term a next b)
  (accumulate * 1 term a next b))

; exercise 1.32b
; accumulate as iterative process, not recursive

(define (accumulate-iter combiner
                         null-value
                         term
                         a
                         next
                         b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (combiner result
                      (term a)))))
  (iter a null-value))

; exercise 1.33

(define (filtered-accumulate combiner
                             null-value
                             term
                             a
                             next
                             b
                             condition)
  (if (> a b)
    null-value
    (combiner (if (condition a) (term a) null-value)
              (filtered-accumulate combiner
                                   null-value
                                   term
                                   (next a)
                                   next
                                   b
                                   condition))))

; exercise 1.33a
; express sum of squares of primes in interval [a, b]

(define (sum-of-squares a b)
  (filtered-accumulate +
                       0
                       identity
                       a
                       inc
                       b
                       prime?))

; exercise 1.33b
; sum of all positive integers i < n such that GCD(i,n) = 1

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (ex1.33b n)
  (define (coprime? i)
    ((= gcd i n) 1))
  (filtered-accumulate + 0 identity 0 inc n coprime?))

