#lang racket
; f(n) = n if n < 3
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3
; (if [predicate] [consequent] [alternate])
; (define ([functionName] [formal parameters]) (body))
; recursive procedure, which creates recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))
         )))
; recursive procedure, which creates tail-recursive procedure (iterative)
(define (f-tail-recur n)
  (define (f-iter n0 n1 n2 counter)
    (when (< counter n)
      (display n0)
      (newline)
      (f-iter n1
              n2
              (+ (* 3 n0)
                 (* 2 n1)
                 n2)
              (+ 1 counter))))
  (f-iter 0 1 2 0))

   
; print f
(define (f-print start finish)
  (when (< start finish)
    (display (f start))
    (newline)
    (f-print (+ 1 start) finish)
      ))