#lang racket

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
  )
)

(define (iter-fact-calculator n counter value)
  (if (> counter n)
      value
      (iter-fact-calculator n (+ counter 1)(* value counter))
  )
)

(define (iter-fact n)
  (iter-fact-calculator n 1 1))

(iter-fact 7)
(factorial 7)
