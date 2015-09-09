#lang racket

; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (negative? d)   ; might be more generic than (> d 0)
      (cons (/ (* n -1) g) (/ (* d -1)))
      (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.2

; constructor
(define (make-segment p1 p2)
  (cons p1 p2))

; selectors
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))

; constructor
(define (make-point x y)
  (cons x y))

; selectors
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment l)
  (make-point (/ (+ (x-point (start-segment l))
                    (x-point (end-segment l)))
                 2)
              (/ (+ (y-point (start-segment l))
                    (y-point (end-segment l)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Exercise 2.3

; first implementation
(define (make-rectangle length width)
  (cons length width))

(define (rectangle-length rectangle)
  (car rectangle))
(define (rectangle-width rectangle)
  (cdr rectangle))

; second implementation
(define (make-rectangle-2 diagonal)
  (cons (start-segment diagonal)
        (end-segment diagonal)))

(define (rectangle-length-2 rectangle)
  (abs (- (x-point (car rectangle))
          (x-point (cdr rectangle)))))
(define (rectangle-width-2 rectangle)
  (abs (- (y-point (car rectangle))
          (y-point (cdr rectangle)))))

; should work with both implementations 
(define (rectangle-area rectangle)
  (* (rectangle-length rectangle)
     (rectangle-width rectangle)))
(define (rectangle-perimeter rectangle)
  (+ (* 2 (rectangle-length rectangle))
     (* 2 (rectangle-width rectangle))))

; Exercise 2.4

; provided alternative procedural representation of pairs
(define (cons-alt x y)
  (lambda (m) (m x y)))
(define (car-alt z)
  (z (lambda (p q) p))) ; can't parse the z piece

; verify (car (cons x y)) yields x
(car-alt (cons-alt 1 2))

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
(define one
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               x)) f) x)))))

; define 'two' as (add-1 one)
(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               (f (((lambda (f)
                      (lambda (x)
                        x)) f) x))))f) x)))))

; Exercise 2.7

; provided
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b)
  (cons a b))

; define selectors
(define (lower-bound i)
  (min (car i) (cdr i)))
(define (upper-bound i)
  (max (car i) (cdr i)))

; Exercise 2.8
(define (sub-interval a b)
  (add-interval a
                (make-interval (* -1 (lower-bound b))
                               (* -1 (upper-bound b)))))

; Exercise 2.9
(define (width-interval i)
  (/ (abs (- (lower-bound i)
             (upper-bound i)))
     2))

