#lang racket

; Exercise 2.24

; Exercise 2.25
(let ([list1 (list 1 3 (list 5 7) 9)]
      [list2 (list (list 7))]
      [list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))])
  (car (cdr (car (cdr (cdr list1)))))
  (car (car list2))
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))))))))

; Exercise 2.26
; predictions:
;
; (1 2 3 4 5 6)
; ((1 2 3) 4 5 6)
; ((1 2 3) (4 5 6))

(let ([x (list 1 2 3)]
      [y (list 4 5 6)])
  (append x y)
  (cons x y)
  (list x y))

; Exercise 2.27
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

(define (flat-list? l)
  (or (not (list? l))
      (null? l)
      (and (not (pair? (car l)))
           (flat-list? (cdr l)))))

(define (deep-reverse l)
  (cond ((not (pair? l)) l)
        ((flat-list? l) (reverse l))
        (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

; Exercise 2.28
(define (fringe l)
  (cond
    ((not (list? l)) (list l))
    ((flat-list? l) l)
    (else (append (fringe (car l)) (fringe (cdr l))))))

; Exercise 2.29
; constructors given
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  ; if the cdr of a given branch (the structure) is a weight (pair)
  ; and not another mobile (list)
  ; then i can return the weight
  ;
  ; therefore, if you give me branches, i can give you weight
  ; how to test if given parameter is a branch?
  ; anything whose car is a number (a pair) is a branch
  (let ([mobile? (lambda (l) (list? (car l)))]
        [branch? (lambda (l) (and (pair? (car l))
                                  (not (list? (car l)))))]
        [branch-weight (lambda (l) (cdr l))])
    (cond
      ((null? mobile) 0)
      ((branch? mobile) (branch-weight mobile))
      ((mobile? mobile) (+ (total-weight (left-branch mobile))
                           (total-weight (right-branch mobile)))))))

; misc
(let* ([l (make-branch 1 2)]
       [r (make-branch 3 4)]
       [m1 (make-mobile l r)]
       [m2 (make-mobile l m1)]
       [m3 (make-mobile m1 m2)])
  (displayln (total-weight m1)))

(define (deep-count l)
  (cond ((not (list? l)) 1)
        ((flat-list? l) (length l))
        (else (+ (deep-count (car l))
                 (deep-count (cdr l))))))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 5 6) (list 7 8)))
(define z (list 1 (list 2 3) (list (list 4 5 (list 6 7 (list 8 9))))))

(define (test-proc proc)
  (display "In:  ")
  (displayln (list 1))
  (display "Out: ")
  (displayln (proc (list 1)))
  (display "In:  ")
  (displayln z)
  (display "Out: ")
  (displayln (proc z))
  (display "In:  ")
  (displayln (list 1 (list 2 3)))
  (display "Out: ")
  (displayln (proc (list 1 (list 2 3))))
  (display "In:  ")
  (displayln (car x))
  (display "Out: ")
  (displayln (proc (car x)))
  (display "In:  ")
  (displayln x)
  (display "Out: ")
  (displayln (proc x))
  (display "In:  ")
  (displayln (list x y))
  (display "Out: ")
  (displayln (proc (list x y)))
  (display "In:  ")
  (displayln (list (list x y) (list x y)))
  (display "Out: ")
  (displayln (proc (list (list x y) (list x y)))))

; (test-proc fringe)
