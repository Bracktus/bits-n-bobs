#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))
  )
)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m)
)

(define (transpose mat)
  (accumulate-n cons '() mat)
)

;Get transpose of n
;For each row in m, do matrix vector multiplication with n^T
;
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
         m)))


(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define matrix-t (transpose matrix))
(define matrix-a (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(matrix-*-vector matrix (list 2 3 4 5))
(matrix-*-matrix matrix-a matrix-a) 


