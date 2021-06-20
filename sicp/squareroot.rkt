#lang racket

(define (abs x)
  (if (> x 0)
      x
      (- x)
  )
)

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))


(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (writeln guess)
  (if (good-enough? guess x)
     guess
    (sqrt-iter (improve guess x) x)
  )
)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
