#lang racket

#|
data abstraction permits us to design programs without becoming enmeshed in
the details of data representations

abstraction preserves the flexibility to experiment with alternative
representations

now introducing 'conventional interfaces', a design principle for working
with data structures
|#

; Section 2.2.3
; Exercise 2.33
; Fill in the missing expressions to complete the following definitions of
; some basic list-manipulation operations as accumulations:

; (accumulate + 0 (list 1 2 3 4 5)) > (+ 1 2 3 4 5) > 15
; accumulate returns 1 expression
; i want that one expression to be a list
; (map square (list 1 2 3 4 5)) > (list (square 1) (square 2) ... )
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (square x) (* x x))

(define (map p sequence)
  ; so as you move from right to left across the sequence, you should
  ; 'accumulate' a list of elements with the p operation applied
  (accumulate (λ (x y)
                (cond ((null? y)
                       x)
                      ((list? y)
                       (append (list (p x)) y))
                      (else
                       (append (list (p x)) (list (p y))))))
              ; the anonymous λ function here takes 2 parameters because
              ; accumulate performs function 'op' on two parameters, 'initial'
              ; and 'sequence'
              ;
              ; and neither x nor y can be defined in terms of 'sequence'?
              ; why not?
              ;
              ; i'd expect you'd be working on some subset of 'sequence'
              ;
              ; x and y, when fed into accumulate, are (car sequence)
              ; and (accumulate op initial (cdr sequence))
              null
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
; this again illustrates that the accumulate function moves from right
; to left

(define (length sequence)
  (accumulate <??> 0 sequence))
