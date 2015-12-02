#lang racket

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

;   (width-interval (add-interval (make-interval a b) (make-interval c d))) ->
;   (width-interval (make-interval (+ a c) (+ b d))) ->
;   (/ (- (+ b d) (+ a c)) 2) ->
;   (/ (+ b d (- a) (- c)) 2) ->
;   (/ (+ (- b a) (- d c)) 2) ->
;   (+ (/ (- b a) 2) (/ (- d c) 2))
;
;   but! this is sum of widths of addends
;   therefore, width of sum is function of widths of addends
;
;   not so for multiplication -
(width-interval (mul-interval (make-interval 1 2)
                              (make-interval 3 4)))
(width-interval (mul-interval (make-interval 1 2)
                              (make-interval 2 3)))
;
;   width of factors consistent, but width of products not
;   therefore width of product not function of widths of factors

; Exercise 2.10
(define (div-interval-2 x y)
  (if (or (= (width-interval x) 0)
          (= (width-interval y) 0))
      (display "Error: divide by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Exercise 2.11
;
; question implies helpful to minimize multiplications
; what about (min x y) func or (let) or (cond)?
; could additional use of said functions cancel gains?
;
; secondary question: this took me forever. how to do fast?
; in many cond preps: put each 'and' condition on own line for
; readability or shorten overall length of define exp.?

(define (mul-interval-2 x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y))
        (a-neg (negative? (lower-bound x)))
        (b-neg (negative? (upper-bound x)))
        (c-neg (negative? (lower-bound y)))
        (d-neg (negative? (upper-bound y))))
    (cond ((and (not a-neg) (not b-neg) (not c-neg) (not d-neg))
           (make-interval (* a c) (* b d)))
          ((and a-neg (not b-neg) (not c-neg) (not d-neg))
           (make-interval (* a d) (* b d)))
          ((and (not a-neg) (not b-neg) c-neg (not d-neg))
           (make-interval (* b c) (* b d)))
          ((and a-neg b-neg (not c-neg) (not d-neg))
           (make-interval (* a d) (* b c)))
          ((and a-neg (not b-neg) c-neg (not d-neg))
           (make-interval (min (* a d) (* b c)) (max (* a c) (* b d))))
          ((and (not a-neg) (not b-neg) c-neg d-neg)
           (make-interval (* b c) (* a d)))
          ((and a-neg b-neg c-neg (not d-neg))
           (make-interval (* a d) (* a c)))
          ((and a-neg (not b-neg) c-neg d-neg)
           (make-interval (* b c) (* a c)))
          ((and a-neg b-neg c-neg d-neg)
           (make-interval (* b c) (* a c))))))

; Exercise 2.12
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (make-center-percent c p)
  (make-interval (- c (* c p))
                 (+ c (* c p))))
(define (percent i)
  (/ (width-interval i)
     (center i)))

; per 2.1.3 'What is Meant by Data?', to test, all i did was wrap constructor
; inside of selector

; Exercise 2.13

(define (exercise2.13 factor-a-center
                      factor-b-center
                      factor-a-percent
                      factor-b-percent
                      percent-step-size
                      test-count)
  (define (iter factor-a-percent factor-b-percent counter bound)
    (if (> counter bound) (if-consequent)
        (if-alternative factor-a-percent factor-b-percent counter bound)))
  (define (if-consequent)
    (display "\n\n*** tests complete ***"))
  (define (if-alternative factor-a-percent factor-b-percent counter bound)
    (display "\n\ntest number ")
    (display counter)
    (display "\n  factor a percent: ")
    (display factor-a-percent)
    (display "\n  factor b percent: ")
    (display factor-b-percent)
    (display "\n  product percent:  ")
    (display (percent (mul-interval (make-center-percent factor-a-center
                                                         factor-a-percent)
                                    (make-center-percent factor-b-center
                                                         factor-b-percent))))
    (display "\n  margin of error:  ")
    (display (- (+ factor-a-percent factor-b-percent)
                (percent (mul-interval (make-center-percent factor-a-center
                                                            factor-a-percent)
                                       (make-center-percent factor-b-center
                                                            factor-b-percent)))))
    (iter (+ factor-a-percent percent-step-size)
          (+ factor-b-percent percent-step-size)
          (+ counter 1)
          bound))
  (iter factor-a-percent factor-b-percent 1 test-count))

; (exercise2.13 5.0 10.0 .01 .02 .01 10)

; so, i had to function decompose my if-alternative because
; if won't accept more than one function
; however! that seems to mean i need to pass parameter
; from function to function to function
; from a coding perspective, is there a better alternative?

; Exercise 2.14
 
; provided
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; went back and forth on whether to define this recursively or use
; 'for' syntactic sugar to iterate
; not entirely sure why recursive procedures are emphasized so much -
; slower, right? more resource intensive
; you can do tail-calls but that only works for racket AFAIK

(define (exercise2.14 counter limit
                      center1 percent1 step-size1
                      center2 percent2 step-size2)
  (define (consequent counter limit)
    (let ((par1-calc (par1 (make-center-percent center1 percent1)
                           (make-center-percent center2 percent2)))
          (par2-calc (par2 (make-center-percent center1 percent1)
                           (make-center-percent center2 percent2))))
      (printf "\ntest ~s\n" (+ counter 1))
      (printf "     r1 percent: ~s\n" percent1)
      (printf "     r2 percent: ~s\n" percent2)
      (printf "     par1 calc:  ~s\n" par1-calc)
      (printf "     par2 calc:  ~s\n" par2-calc)
      (printf "     error:      ~s\n"
              (sub-interval par1-calc par2-calc))
      (exercise2.14 (+ counter 1) limit
                    center1 (+ percent1 step-size1) step-size1
                    center2 (+ percent2 step-size2) step-size2)))
  (cond ((< counter limit) (consequent counter limit))))
(exercise2.14 0 10
              5 .01 .001
              10 .02 .001)

; if you put the definition of consequent function after main
; cond expression, program fails - program fails to id any expressions
; following definitions

; Exercise 2.15
