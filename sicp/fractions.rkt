#lang racket

(define (cont-frac n d k)
  (define (loop i)
      (if (= i k)
          (/ (n i) (d i))
          (/ (n i) (+ (d i) (loop (+ i 1))))
      )
  )
  (loop 0)
)

(define (cont-frac-iter n d k)
  (define (loop i result)
    (if (= i -1)
        result
        (loop (- i 1) (/ (n i) (+ (d i) result)))
    )
  )
  (loop (- k 1) (/ (n k) (d k)))
)

(define (euler-expansion k f)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 1)
        (/ (* (+ i 2) 2) 3)
        1
    )
  )
  (f n d k)
)

(define (tan-x x k f)
  (define (n i)
    (if (= i 0)
        x
        (* x x -1)
    )
  )
  (define (d i)
    (+ (* 2 i) 1)
  )
  (f n d k)
)

(display "1/phi = ")
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
            10)

(display "1/phi = ")
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10)

(display "e - 2 = ")
(euler-expansion 20 cont-frac)

(display "e - 2 = ")
(euler-expansion 20 cont-frac-iter)

(display "tan(0.8) = ")
(tan-x 0.8 100 cont-frac)
(display "tan(0.8) = ")
(tan-x 0.8 100 cont-frac-iter)


