#lang racket

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else 
           (error "Argument not 0 or 1:
                   CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(define k (cons 2 3))
(car k)
(cdr k)

;Returns a 2 arg function *m* with arguments *x* and *y*
(define (cons2 x y)
  (lambda (m) (m x y))
)

;Takes in a 2 arg function *z* and returns the first argument *p*
(define (car2 z)
  (z (lambda (p q) p))
)

;Takes in a 2 arg function *z* and returns the second argument *q*
(define (cdr2 z)
  (z (lambda (p q) q))
)

;We can represent pairs using a single integer
;the pair (a,b) = 2^a * 3^b
(define (exp base n) 
    (define (iter x result) 
      (if (= 0 x) 
          result 
          (iter (- x 1) (* base result))
      )
    ) 
    (iter n 1)
) 
  
(define (cons3 a b)
  (* (exp 2 a) (exp 3 b))
)

(define (num-of-divisions-till-undivisible n divisor)
  (define (iter num times)
    (if (not (divisible-by-k? num divisor))
        times
        (iter (/ num divisor) (+ times 1))
    )
  )
  (iter n 0)
)

(define (divisible-by-k? n k) 
  (= (remainder n k) 0)
)

(define (extract n idx)
  (define (get-val div)
    (num-of-divisions-till-undivisible n div)
  )
  
  (cond ((= 0 idx) (get-val 2))
        ((= 1 idx) (get-val 3)))
)

(define (car3 pair)
  (extract pair 0)
)

(define (cdr3 pair)
  (extract pair 1)
)

(newline)
(display "5,7")
(newline)
(define j (cons3 5 7))
(car3 j)
(cdr3 j)

