#lang racket

(define (iter-sum term a next b)
  (define (iter a result)
      (if (> a b)
          result
          ( iter (next a) (+ result (term a)) )
      )
  )
  (iter a 0)
)

(define (recur-sum term a next b)
  (if (> a b)
      0
      (+ (term a) (recur-sum term (next a) next b))
  )
)

(define (recur-prod term a next b)
    (if (> a b)
        1
        (* (term a) (recur-prod term (next a) next b))
    )
)

(define (iter-prod term a next b)
  (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (* result (term a)))
      )
  )
  (iter a 1)
)

(define (cube a b)
  (define (term x)
    (* x x x))
  (define (next n)
    (+ 1 n))
  (iter-sum term a next b)
)

(define (factorial a b)
  (define (term x) x)
  (define (next n)
    (+ 1 n)
  )
  (iter-prod term a next b)
)

(define (pi n)

  (define (square x) (* x x)) 
  (define (term n)
    (cond ((even? n) (/ (+ n 2) (+ n 1)))
          (else (/ (+ n 1) (+ n 2)))
    )
  )
  (define (next x) (+ 1 x))
  (* (iter-prod term 1 next n) 4)
  (* (recur-prod term 1 next n) 4)
)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))
  )
)

(define (filtered-accumulate combiner null-value term a next b filter)
    (cond ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (combiner null-value (accumulate combiner null-value term (next a) next b))) 
  )
)

(define (prime? n)
  (define (square x) (* x x))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m))m))
          (else(remainder (* base (expmod base (- exp 1) m))m))
    )
  )

  (define (iter n a)
      (cond ((= a 1) true) 
            ((not (= (expmod a n n) (remainder a n))) false)
            (else (iter n (- a 1)))
      ) 
  )
  (iter n n)
)

(define (sum-of-squared-primes a b)
  (define (term n)
    (* n n))
  (define (next n)
    (+ n 1))
  (filtered-accumulate + 0 term a next b prime?)
)

(sum-of-squared-primes 2 300) ;1 is treated as prime by the test so start at 2

