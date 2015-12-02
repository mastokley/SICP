; 1.3.4 Procedures as Returned Values
;
; passing procedures as arguments vs.
; creating procedures which return procedures
;
; average damping

#lang racket

(require "sec1.3.3.rkt")

(define (average-damp f)    ; formal parameter f is procedure
  (lambda (x)               ; returns anon procedure produced
    (average x (f x))))     ; by lambda

((average-damp square) 10)  ; a combination whose operator is
; itself a combination

; using the average-damp procedure, we might reformulate
; the sqrt function as

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; since we have abstracted, we can reuse
; here is a cube root in terms of average-damping

(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; express the idea of a derivative
; note a derivative is a process (function), applied to a function,
; which returns another function (procedure)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x))
       dx)))
(define dx 0.00001)

; deriv provides a great example of a procedure as a returned value

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; Exercise 1.40

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

; Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

(define (inc i)
  (+ i 1))

; Exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
    f
    (compose f
             (repeated f (- n 1)))))

; Exercise 1.44

(define (smooth f)
  (define dx .00001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))


; Exercise 1.45
; 
; i have an 'average-damp' function
; i have a 'repeated' function
; how did we attempt to compute square roots?
; i have a 'fixed-point' function

;(define (sqrt x)
;  (fixed-point (lambda (y) (/ x y))
;               1.0)) ;first guess

; then we improve by feeding y^2 = x into average-damp

;(define (sqrt x)
;  (fixed-point (average-damp (lambda (y) (/ x y)))
;               1.0))

(define (nth-root x n)
  (fixed-point (repeated (average-damp (lambda (y)
                                         (/ x
                                            (expt y (- n 1)))))
                         (log2 n))
               1.0))

(define (log2 x)
  (/ (log x) (log 2)))

; Exercise 1.46

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve-guess)
       (improve-guess guess)))))
    ; can a lambda function, being anonymous,
    ; be recursively defined?

; rewrite sqrt procedure from section 1.1.7

; original
; (define (sqrt-iter guess x)
;   (if (good-enough? guess x)
;     guess
;     (sqrt-iter (improve guess x)
;                x)))
; (define (improve guess x)
;   (average guess (/ x guess)))
; (define (average x y)
;   (/ (+ x y) 2))
; (define (good-enough? guess x)
;   (> (abs (- (square guess) x)) .001))
; (define (sqrt x)
;   (sqrt-iter 1.0 x))
; 
; rewrite
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ;  (define (average x y)
  ;    (/ (+ x y) 2))
  ((iterative-improve good-enough? improve-guess) 5.0))

; rewrite fixed-point procedure from 1.3.3

; original

; (define (tolerance .00001))
; (define (fixed-point f first-guess)
;   (define (close-enough? v1 v2)
;     (< (abs (- v1 v2)) tolerance))
;   (define (try guess)
;     (let ((next (f guess)))
;       (if (close-enough? guess next)
;         next
;         (try next))))
;   (try first guess))

; rewrite

(define (fixed-point f first-guess)
  (define (good-enough? guess)  ; rewrite with one parameter only
    (< (abs (- guess (f guess)) tolerance)))
  (define tolerance .00001)
  (define (improve-guess guess)
    (f guess))
  ((iterative-improve good-enough? improve-guess) first-guess))
