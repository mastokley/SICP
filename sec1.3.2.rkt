; 1.3.2 Constructing Procedures Using Lambda

#lang racket

; (lambda (x) (+ x 4))
; 
; (lambda (x)
;   (/ 1.0 
;      (* x 
;             (+ x 2))))
; 
; (define (pi-sum a b)
;   (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
;        a
;        (lambda (x) (+ x 4))
;        b))
; 
; (define (integral f a b dx)
;   (* (sum f
;           (+ a (/ dx 2.0))
;           (lambda (x) (+ x dx))
;           b)
;      dx))
; 
; (define (plus4 x) (+ x 4))
; 
; (define plus4 (lambda (x) (+ x 4)))
; 
; ((lambda (x y z) 
;    (+ x y (square z))) 
;  1 2 3)
; 
; (define (f x y)
;   (define (f-helper a b)
;     (+ (* x (square a))
;        (* y b)
;        (* a b)))
;   (f-helper (+ 1 (* x y))
;             (- 1 y)))
; 
; (define (f x y)
;   (let ((a (+ 1 (* x y)))
;         (b (- 1 y)))
;     (+ (* x (square a))
;        (* y b)
;        (* a b))))

; Exercise 1.34

(define (f g)
  (g 2))

(define (square x)
  (* x x))

; if we ask the interpreter to evaluate (f f), it fails
; f is a procedure, which expects the argument g
; we provided the argument (f), which is defined as the value 2
; so is f a procedure or a value?...

; why is the eval of (f square) => 4?
; (define (f g)
;   (g 2)) =>
; (f square) => (square 2) => 4

;
; f is a procedure which says, perform procedure g on 2
; if you feed it into itself, it's going to break
; use substitution model to explain failure

; (f f) => (f 2) => (2 2)
;
; 2 is not a procedure
;
; is this illustrating the blurred lines between procedures and data?
;
; is g a procedure, which is performed on 2 or is g defined as 2?
; it's the former, because we're missing the define keyword
