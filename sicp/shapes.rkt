#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (square n) (* n n))

(define (average-points a b)
  (make-point (average (x-point a) (x-point b))
              (average (y-point a) (y-point b)))
)

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (midpoint-segment segment)
  (let ((point-a (start-segment segment))
        (point-b (end-segment segment)))
    (average-points point-a point-b)
  )
)

(define (segement-length segment)
  (let ((point-a (start-segment segment))
        (point-b (end-segment segment)))

    (let ((point-a-x (x-point point-a))
          (point-a-y (y-point point-a))
          (point-b-x (x-point point-b))
          (point-b-y (y-point point-b)))

      (sqrt (+ (square (- point-a-x point-b-x))
               (square (- point-a-y point-b-y))))
    )
  )
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define seg (make-segment (make-point 2 3) 
                          (make-point 10 15))) 
  
;(print-point (midpoint-segment seg))

;A rectangle is defined by its top left point and its bottom right point

(define (make-rect point-a point-b)
  (cons point-a point-b))

(define (rect-top-left rect)
  (car rect)
)

(define (rect-top-right rect)
  (let ((top-left (rect-top-left rect))
        (bottom-right (rect-bottom-right rect)))

    (make-point (x-point bottom-right)
                (y-point top-left))
  )
)

(define (rect-bottom-left rect)
  (let ((top-left (rect-top-left rect))
        (bottom-right (rect-bottom-right rect)))

    (make-point (x-point top-left)
                (y-point bottom-right))
  )
)

(define (rect-bottom-right rect)
  (cdr rect)
)

(define (rect-height rect)
  (segement-length (make-segment (rect-top-left rect) (rect-bottom-left rect)))
)

(define (rect-width rect)
  (segement-length (make-segment (rect-bottom-left rect) (rect-bottom-right rect)))
)


;Procedures to compute perimeter and area

(define (area rect)
  (* (rect-width rect) (rect-height rect))
)

(define (perimeter rect)
  (+ (* 2 (rect-width rect)) (* 2 (rect-height rect)))
)

(define test-rect
  (make-rect (make-point 5 5)
             (make-point 9 3))
)

(display "perimeter: ")
(perimeter test-rect)
(display "area: ")
(area test-rect)


