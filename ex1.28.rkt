#lang racket

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square n)
  (* n n))

(define (nontrivial-square-root? a n)
  (and (not (or (= a 1)
                (= a (- n 1))))
       (= (square a) 
          (remainder 1 n))))

(define (congruent-modulo-n x y n)
  (= (remainder x n)
     (remainder y n)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2)
     0))

(define (square-check x m)
  (if (and (not (or (= x 1)
                    (= x (- m 1))))
           (= (remainder (* x x) m) 
              1))
    0
    (remainder (* x x) m)))
