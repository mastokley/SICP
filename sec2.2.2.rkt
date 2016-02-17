#lang racket

; Section 2.2.2 Hierarchical Structures

; Exercise 2.24
; interpreter prints:
; '(1 (2 (3 4)))

; Exercise 2.25
;(let ([list1 (list 1 3 (list 5 7) 9)]
;      [list2 (list (list 7))]
;      [list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))])
;  (car (cdr (car (cdr (cdr list1)))))
;  (car (car list2))
;  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))))))))

; Exercise 2.26
; predictions:
;
; (1 2 3 4 5 6)
; ((1 2 3) 4 5 6)
; ((1 2 3) (4 5 6))
;
;(let ([x (list 1 2 3)]
;      [y (list 4 5 6)])
;  (append x y)
;  (cons x y)
;  (list x y))

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
  (cond ((not (list? l)) (list l))
        ((flat-list? l) l)
        (else (append (fringe (car l)) (fringe (cdr l))))))

; Exercise 2.29
; constructors given
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; a. write corresponding selectors
; since mobiles and branches are more like pairs than lists, must use cadr
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b. define procedure 'total-weight'
#|
solved via function decomposition (branch?, mobile? predicates)

when navigating a tree you must first cut it up (left-branch, right-branch)
and then determine which piece of the tree you have

book defines this as constructors and selectors, but i'm finding it helpful
to have predicates as well
|#

(define (branch? branch)
  (and (not (pair? (branch-length branch)))
       (pair? branch)))
(define (mobile? mobile)
  (and (pair? mobile)
       (pair? (left-branch mobile))
       (pair? (right-branch mobile))))
(define (total-weight mobile)
  (cond ((null? mobile)
         0)
        ((and (branch? mobile)
              (not (mobile? (branch-structure mobile))))
         (branch-structure mobile))
        ((and (branch? mobile)
              (mobile? (branch-structure mobile)))
         (total-weight (branch-structure mobile)))
        ((mobile? mobile)
         (+ (total-weight (left-branch mobile))
            (total-weight (right-branch mobile))))))

; c. define predicate 'balanced?'
;
; mobile is said to be balanced if torque applied by its top-left branch is
; equal to that applied by its top-right branch
; AND
; each of sub-mobiles is balanced

(define (torque branch)
  (* (branch-length branch)
     (total-weight branch)))
(define (balanced? mobile)
  (cond ((mobile? mobile)
         (and (= (torque (left-branch mobile))
                 (torque (right-branch mobile)))
              (balanced? (left-branch mobile))
              (balanced? (right-branch mobile))))
        ((and (branch? mobile)
              (mobile? (branch-structure mobile)))
         (balanced? (branch-structure mobile)))
        ((and (branch? mobile)
              (not (mobile? (branch-structure mobile))))
         #t)))

; couple thoughts on this one... could not progress without
; a) diagramming 'mobile' data structures as trees;
; b) defining mobile? and branch? predicates
; c) thinking through how to ID which part of tree function is dealing with at any given time
; also, that last consequent (simply #t) in the cond statement seems clumsy
; it's trying to test whether the sub-mobiles of the original mobile are balanced, but
; what if the original mobile has no sub-mobiles?
; maybe #t is the equivalent of returning 0 if null on total-weight function

; d. Suppose we change the representation of mobiles from list to cons:
(define (make-mobile-2 left right)
  (cons left right))
(define (make-branch-2 length structure)
  (cons length structure))

; I'd need to change the right-branch selector to use cdr instead of cadr:
(define (right-branch-2 mobile) (cdr mobile))

; likewise with the branch-structure selector:
(define (branch-structure-2 branch) (cdr branch))

; but that's it! total-weight and balanced? are safely beyond the abstraction barrier.

; Exercise 2.30
; copying exact definition of scale-tree from book
; do not understand function

; this version defined directly
(define (square-tree-1 tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (* tree tree))
        (else
         (cons (square-tree-1 (car tree))
               (square-tree-1 (cdr tree))))))

; this version defined with higher-order function map
(define (square-tree-2 tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (* sub-tree sub-tree)))
       tree))

; Exercise 2.31
; so what this does... is... for each element in list
; if element is a pair (i. e. a sub list), it applies tree map to it
; otherwise, it acts as a simple map function
(define (tree-map function tree)
  (map (λ (sub-tree)
         (if (pair? sub-tree)
             (tree-map function sub-tree)
             (function sub-tree)))
       tree))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))

; Exercise 2.32
; define the function 'subsets', which takes parameter 's'
(define (subsets s)
  ; what is s? if null, return the null list
  (if (null? s)
      (list null)
      ; otherwise, define 'rest' as the subsets of (cdr s)
      (let ((rest (subsets (cdr s))))
        ; and append to that (the rest)
        ; a list generated by
        ; mapping which function to (cdr s)?
        ; tried car, cdr, cadr
        ; not sure if this works for a nested list (tree) or not
        ; tried list, append, subsets, map
        (append rest (map <??> rest)))))

(define list1 (list 1 (list 2 3) (list 4 (list 5 6) 7) 8))
(define list2 (list 1 2 3))
