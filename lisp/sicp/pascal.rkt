#lang racket

(define (pascal row col)
  (if (or (= row col) (= row 1))
      1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))
  )
)

(pascal 3 2)
