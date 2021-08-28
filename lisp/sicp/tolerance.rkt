#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (- (upper-bound y) (lower-bound y)) 0)
      (error "Division error (interval spans 0)" y)
      (mul-interval x 
                    (make-interval 
                        (/ 1.0 (upper-bound y)) 
                        (/ 1.0 (lower-bound y)))
      )
  )
)

(define (opposite-pair? x y)
  (if (< (* x y) 0)
      #t
      #f
  )
)

(define (positive-pair? x y)
  (if (and (not (opposite-pair? x y)) (positive? x))
      #t
      #f
  )
)

(define (negative-pair? x y)
  (if (and (not (opposite-pair? x y)) (negative? x))
      #t
      #f
  )
)

(define (mul-interval2 x y)
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))

    (cond ((and (negative-pair? x0 x1) (negative-pair? y0 y1))
           (make-interval (* x1 y1) (* x0 y0)))

          ((and (negative-pair? x0 x1) (opposite-pair? y0 y1))
           (make-interval (* x0 y1) (* x1 y0)))

          ((and (negative-pair? x0 x1) (positive-pair? y0 y1))
           (make-interval (* x0 y1) (* x0 y0)))

          ((and (positive-pair? x0 x1) (positive-pair? y0 y1))
           (make-interval (* x1 y1) (* x0 y0)))

          ((and (positive-pair? x0 x1) (opposite-pair? y0 y1))
           (make-interval (* x1 y1) (* x1 y1)))

          ((and (positive-pair? x0 x1) (negative-pair? y0 y1))
           (make-interval (* x1 y0) (* x0 y1)))

          ((and (opposite-pair? x0 x1) (negative-pair? y0 y1))
           (make-interval (* x1 y0) (* x0 y0)))

          ((and (opposite-pair? x0 x1) (opposite-pair? y0 y1))
           (make-interval (min (* x0 y1) (* x1 y0)) (max (* x0 y0) (* x1 y1))))
          
          ((and (opposite-pair? x0 x1) (positive-pair? y0 y1))
           (make-interval (* x0 y1) (* x1 y1)))
    )
  )
)

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define (make-interval a b) 
  (if (< a b)
      (cons a b)
      (cons b a)
  )
)

(define (lower-bound x)
  (car x)
)

(define (upper-bound x)
  (cdr x)
)

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2)
)

;(mul-interval (make-interval 1 2) (make-interval 2 3))
;(mul-interval2 (make-interval 1 2) (make-interval 2 3))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center2 i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width2 i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (make-center-percent c p)
  (let ((width (* (/ c 100) p)))
    (make-center-width c width)
  )
)

(define (percent interval)
  (* (/ (width2 interval) (center2 interval)) 100)
)


