; 1.3.3 Procedures as General Methods

#lang racket

; methods of abstraction:
; compound procedures (defining new procedures e. g. 'square') vs. 
; higher-order procedures (procedures as arguments/parameters)
;
; i think lambda represents reverse functional decomposition
; does let represent the same thing?

(provide (all-defined-out))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

;(define (sqrt x)
;  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

; Exercise 1.35 show that the golden ratio is a fixed point of the
; transformation x => 1 + 1/x, and use this fact to compute the 
; golden ratio by means of the fixed point procedure

;(define (golden-ratio)
;  (fixed-point (lambda (x) (+ 1
;                              (/ 1 x)))
;               1.0))
; Exercise 1.36 modify fixed-point to show approximations

;(define tolerance 0.00001)
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (display guess)
;    (newline)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;       next
;        (try next))))
;  (try first-guess))

;(define (ex1.36)
;  (fixed-point (lambda (x)
;                 (average x
;                          (/ (log 1000)
;                             (log x))))
;               2.0))

; Exercise 1.37
;
; k-term finite continued fraction
; n, d are procedures of one argument (term index i) that return
; N_i and D_i of the terms of the continued fraction

(define (cont-frac n d k)
  (define (helper i)
    (/ (n i)
       (+ (d i)
          (cond ((not (< k i))
                 (helper (+ i 1)))
                ((< k i)
                 0)
                ))))
  (helper 1))

; is the above process iterative or recursive? generate the other 
; either way, it's a recursively defined procedure.

; does the evolved process build up a chain of deferred operations?
; yes! i can't resolve the sum of (d i) and ... until I resolve
; the latter
;
; a shape of expansion followed by contraction means this is a
; recursive process - a linearly recursive process, because it
; grows just as n
;
; i think i need to rearrange the continued fraction as a series
; of sums in order to express as an iterative process

; Exercise 1.37b

(define (cont-frac-iter n d k)
  (define (iter i sum)
    (if (= i 0)
      sum
      (iter (- i 1)
            (/ (n i)
               (+ (d i)
                  sum)))))
  (iter (- k 1) (/ (n k)
                   (d k))))

; Exercise 1.38

(define (euler-cont-frac k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((= (modulo (+ i 1)
                                 3) 
                         0)
                      (* (+ i 1)
                         (/ 2 3)))
                     (else 1)))
             k))

; Exercise 1.39

(define (square x)
  (* x x))

(define (tan-cf1 x k)
  (/ x
     (cont-frac-iter (lambda (x) (- (square x)))    ; this doesn't
                                                    ; work - not same
                                                    ; x because
                                                    ; hyper-local
                     (lambda (i) (- (* i 2) 1))
                     k)))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1) x (- (square x))))
  (define (d k) (- (* k 2) 1))
  (cont-frac n d k))
