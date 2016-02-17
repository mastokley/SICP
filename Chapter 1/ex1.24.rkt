#lang racket

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100)
         (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n) (= n (smallest-divisor n)))

(define (even? n) (= (remainder n 2) 0))

(define (search-for-primes start-range end-range)
  (if (even? start-range)
      (search-for-primes (+ 1 start-range) end-range)
      (cond ((< start-range end-range) (timed-prime-test start-range)
             (search-for-primes (+ 2 start-range) end-range)))))

; prime         time elapse     predicted time
; ---------------------------------------------
; 1009          0.0078125000000
; 1013          0.0080566406250
; 1019          0.0070800781250
; 10007         0.0139160156250 0.02470529422007
; 10009         0.0141601562500 0.02547733466444
; 10037         0.0141601562500 0.02238917288693
; 100003        0.0458984375000 0.04400630532949
; 100019        0.0439453125000 0.04477834577387
; 100043        0.0439453125000 0.04477834577387
; 1000003       0.1369628906250 0.14514360354288
; 1000033       0.1359863281250 0.13896727998787
; 1000037       0.1350097656250 0.13896727998787

(define (next n)
  (cond ((= 2 n) 3)
        (else (+ n 2))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(provide (all-defined-out))
