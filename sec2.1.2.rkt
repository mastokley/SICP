#lang racket
; Exercise 2.2

; constructor
(define (make-segment p1 p2)
  (cons p1 p2))

; selectors
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

; constructor
(define (make-point x y) (cons x y))

; selectors
(define (x-point p) (car p))
(define (y-point p) (cdr p))

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
(define (make-rectangle length width) (cons length width))

(define (rectangle-length rectangle) (car rectangle))
(define (rectangle-width rectangle) (cdr rectangle))

; second implementation
(define (make-rectangle-2 diagonal)
  (cons (start-segment diagonal) (end-segment diagonal)))

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
