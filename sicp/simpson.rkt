#lang racket

(define (simpson f a b n)
  
  (define h (/ (- b a) n))

  (define (even? n)
    (= (remainder n 2 ) 0))

  (define (y n)
    (f (+ a (* n h))))

  (define (simpson-term f n)
    (cond ((even? n) (* 2 (y n)))
          (else (* 4 (y n)))
    )
  )

  (define (try-it f n c) 
    (if (< n 0)
        c
        (try-it f (- n 1) (+ c (simpson-term f n)))
    )
  )
  (* (try-it f n 0) (/ h 3))
)

(define (cube x)
  (* x x x))

(simpson cube 2 7 10000)
