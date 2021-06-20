#lang racket

(define (exp b n a)
  (cond ((= n 0) a)
        ((even? n) (exp (square b) (/ n 2) a))
        (else (exp b (- n 1) (* a b)))
  )
)

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(exp 3 7 1)


