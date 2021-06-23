#lang racket

(define (average a b) (/ (+ a b) 2))

(define (close-enough? x y tolerance)
        (< (abs (- x y)) tolerance))

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
          (if (close-enough? neg-point pos-point)
              midpoint
              (let ((test-value (f midpoint)))
                   
                    (cond [(positive? test-value) 
                           (search f neg-point midpoint)]

                          [(negative? test-value) 
                           (search f midpoint pos-point)]

                          (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
        
        (cond ((and (negative? a-value) (positive? b-value))
              (search f a b))

              ((and (negative? b-value) (positive? a-value))
              (search f b a))
                
              (else (error "Values are not of opposite sign" a b))
        )
  )
)


(define (negative? n)
    (cond ((> n 0) false)
          ((< n 0) true)
    )
)

(define (positive? n)
    (not (negative? n))
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)

    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))

    (define (try guess)
        (let ((next (f guess)))
             (display next)
             (newline)
             (if (close-enough? guess next)
                  next
                 (try next)
             )
        )
    )
    
    (try first-guess)
)

(define (newtons-method f x)
    (define (derivative f x h) 
        (/ (- (f (+ x h)) (f (- x h))) (* 2 h)))

    (define (loop guess)
        (let ((next (- guess (/ (f guess) (derivative f guess 0.1)))))
            (if (close-enough? next guess 0.001)
                 next
                 (loop next)
            )
        )
    )
    (loop x)
)

(define (cubic a b c)
  (define (power num n)
    (if (= n 1)
        num
        (* num (power num (- n 1)))
    )
  )
  (lambda (x) (+ (power x 3) (* a (power x 2)) (* b x) c))
)

(newtons-method (cubic 1.0 2.0 3.0) 1)

(newline)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 3.0)
(newline)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 3.0)
(newline)


