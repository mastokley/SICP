#lang racket

; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; Exercise 2.18
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination l) (car l))
(define (except-first-denomination l) (cdr l))
(define (no-more? l) (null? l))

; "Does the order of the coin list matter?" Apparently not.
; Do not know why it would.


; Exercise 2.20
(define (same-parity x . y)
  (let ([parity-test (if (even? x)
                         (lambda (n) (even? n))
                         (lambda (n) (odd? n)))])
    (define (helper input output)
      (if (null? input)
          output
          (helper (cdr input)
                  (if (parity-test (car input))
                      (append output (list (car input)))
                      output))))
    (helper (append (list x) y) '())))

; Exercise 2.21
(define (square x)
  (* x x))

(define (square-list-1 items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map square items))

; Exercise 2.22
; The function below returns the squared list in reverse order because
; it cons's the each squared element to the front of the output list.

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

#|
The function below returns nested pairs. This is because cons does not treat its
two arguments interchangeably, for the same reason that car and cdr cannot be
interchanged. Car returns the first element of a list; cdr returns the remaining
elements. If you pass a list to cons as the first argument, it will be treated
as a single element in the returned list, such that it can be selected back out
as a single element by car. On the other hand, if you pass a list to cons as the
second argument, it will be treated as a list such that it can be selected as a
list of elements by cdr.
|#

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

; Exercise 2.23
(define (for-each proc l)
  (cond ((not (null? l))
         (proc (car l))
         (for-each proc (cdr l)))))
