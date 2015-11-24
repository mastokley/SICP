#lang racket

; Exercise 2.4

; provided alternative procedural representation of pairs
(define (cons-alt x y)
  (lambda (m) (m x y))) ; can't parse m
(define (car-alt z)
  (z (lambda (p q) p))) ; can't parse the z piece

; verify (car (cons x y)) yields x
; (car-alt (cons-alt x y))
; (car-alt (lambda (m)
;            (m x y)))
; ((lambda (m)
;    (m x y))
;  (lambda (p q) p))
; ((lambda (p q) p) x y))
; (x)

; what is corresponding definition of cdr?
(define (cdr-alt z)
  (z (lambda (p q) q))) ; blindly adapting provided car def

; Exercise 2.5

(define (cons-expt-alt a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car-expt-alt z)
  (define (iter n a-count)
    (cond ((= (modulo n 3) 0) (iter (/ n 3) a-count))
          ((= (modulo n 2) 0) (iter (/ n 2) (+ a-count 1)))
          (else a-count)))
  (iter z 0))
(define (cdr-expt-alt z)
  (define (iter n b-count)
    (cond ((= (modulo n 2) 0) (iter (/ n 2) b-count))
          ((= (modulo n 3) 0) (iter (/ n 3) (+ b-count 1)))
          (else b-count)))
  (iter z 0))

; Exercise 2.6

; given Church numerals
(define zero
  (lambda (f)
    (lambda (x)
      x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; define one as (add-1 zero)
; not sure to what extent these nested lambdas
; can/should collapse
;
; i've done maybe 2 steps of substitution
;
; not sure why many of these functions, which are defined as
; requiring parameters, are not provided any parameters
;
; should i collapse any function that has been provided with
; a parameter?

; (define one add-1 zero)

; (define one
;   (lambda (f)
;     (lambda (x)
;      (f ((n f) x))))
;   (lambda (f)
;     (lambda (x)

; (define one
;   (lambda (f)
;     (lambda (x)
;       (f (((lambda (f)
;              (lambda (x)
;                x)) f) x)))))

; define 'two' as (add-1 one)
(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               (f (((lambda (f)
                      (lambda (x)
                        x)) f) x))))f) x)))))
