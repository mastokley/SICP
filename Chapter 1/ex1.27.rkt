; Fermat's Little Theorem: If [n] is a prime number and [a] is any positive
; integer less than [n], then [a] raised to the [n]th power is congruent to
; [a modulo n].
;
; (Two numbers are said to be congruent modulo [n] if they both have the same
; remainder when divided by [n]. The remainder of a number [a] when divided by [n]
; is also referred to as the remainder of [a modulo n], or simply as [a modulo n].)

#lang racket

; computes exponential of a number modulo another number
;

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

(define (Carmichael-number? n)
  (define (test n a)
    ; (newline)
    ; (display a)
    (cond ((= (+ a 1) n)
           (display " -- Success")))
    (cond ((and (= (expmod a n n)
                   (remainder a n))
                (< (+ a 1) n))
           (test n (+ a 1)))))
  (test n 1))

