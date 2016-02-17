#lang racket

; linear recursive process
; (recursive procedure 'evolves' a linear recursive process)
; theta(n) steps
; theta(n) space
(define (expt b n)
  (if (= n 0) 1 (* b (expt b (- n 1)))))

; equivalent linear iteration
; (recursive procedure 'evolves' a linear-iterative process)
; theta(n) steps
; theta(1) space
(define (expt2 b n) (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; successive squaring method
; still a linear-recursive process (not linear-iterative)
; or would you call it a logarithmic-recursive process?
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))

; equivalent iterative exponentiaton process
; uses successive squaring and a logarithmic number of steps
; this is an iterative process that uses an 'invariant quantity'
; , which remains the same from one iteration to the next
; that invariant quantity is (+ currentPower remainderPower)
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
          (else
           (ssExptIter base
                       (* base product)
                       (+ currentPower 1)
                       (- remainderPower 1)))))
  (ssExptIter b b 1 n))
