#lang racket

(define (double func)
  (lambda (x) (func (func x)))
)

(define (inc n)
  (+ n 1)
)

(define (square n)
  (* n n)
)

(define (compose f g)
  (lambda (x) (f (g x)))
)

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))
    )
)

(define (smooth f x dx)
  (let ((f1 (f (- x dx)))
        (f2 (f x))
        (f3 (f (+ x dx))))
    (/ (+ f1 f2 f3) 3)
  )
)

(define (n-fold-smooth f x dx n)
  (repeated (smooth f x dx) n)
)

((repeated square 2) 5)
(smooth (lambda (x) (+ 2 (* x x))) 5 2)


