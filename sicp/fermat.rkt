#lang racket

;To implement the Fermat test, we need a procedure that computes the exponential of a number modulo another number:

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))m))
        (else(remainder (* base (expmod base (- exp 1) m))m))
  )
)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
  )
)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;Write a procedure that takes an integer n and tests whether a^n is congruent to a modulo n for every a<n, and try your procedure on the given Carmichael numbers.

(define (prime-test n)
  (define (iter n a)
      (cond ((= a 1) true) 
            ((not (= (expmod a n n) (remainder a n))) false)
            (else (iter n (- a 1)))
      ) 
  )
  (iter n n)
)

(prime-test 23)
(prime-test 10)
(prime-test 561)

