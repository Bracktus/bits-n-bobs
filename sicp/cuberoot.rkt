#lang racket

(define (abs x)
  (if (> x 0)
      x
      (- x)
  )
)

(define (square x)
  (* x x))

(define (good-enough? oldGuess guess)
  ( < ( abs (/ (- oldGuess guess) guess)) 0.0000001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-iter oldGuess guess x)
  (writeln guess)
  (if (good-enough? oldGuess guess)
    guess
    (cube-iter guess (improve guess x) x)
  )
)

(define (cube-root x)
  (cube-iter 7.0 1.0 x))

(cube-root 59)

