#lang racket
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;
;     r1c1
;   r2c1 r2c2
;r3c1 r3c2 r3c3
;
;rNcN = (+ r(- n 1)c(- n 1) r(- n 1)c(+ n 1))
;unless rN = 1 or rN = cN, in which case rNcN = 1
;
; this is an example of tree recursion

(define (pascalCell r c)
  (cond ((= r 1) 1)
        ((= c 1) 1)
        ((= r c) 1)
        (else (+ (pascalCell (- r 1) (- c 1))
                 (pascalCell (- r 1) c)))))
