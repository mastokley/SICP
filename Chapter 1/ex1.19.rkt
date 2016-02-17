#lang racket
; Exercise 1.19
(define (fib n) (fibIter 1 0 0 1 n))
(define (fibIter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fibIter a
                  b
                  (+ (* p p) (* q q))
                  (+ (* 2 p q) (* q q))
                  (/ count 2)))
        (else (fibIter (+ (* b q) (* a q)
                          (* a p)) (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
