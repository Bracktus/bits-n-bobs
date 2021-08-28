#lang racket

(define unsorted '(9 8 5 7 3 1 2))

;'(1 4 5) '(2 8 8)

(define (merge l1 l2)
  (let loop ([l1 l1]
             [l2 l2]
             [acc '()])
    (cond
      ;if l1 is empty, add l2 to the end
      [(null? l1) (foldl cons l2 acc)]
      ;if l2 is empty, add l1 to the end
      [(null? l2) (foldl cons l1 acc)] 
      [else (if (> (car l1) (car l2))
                ;add l2 to the list
                (loop l1 (cdr l2) (cons (car l2) acc))
                ;add l1 to the list
                (loop (cdr l1) l2 (cons (car l1) acc)))])))
    

(define (subdivide u-list)
  (let ([div-line (quotient (length u-list) 2)])
    (cons (take u-list div-line) 
          (drop u-list div-line))))

(define (mergesort u-list)
  (if (= (length u-list) 1)
      u-list
      (let* ([both  (subdivide u-list)]
             [left  (car both)]
             [right (cdr both)])
        (merge (mergesort left)
               (mergesort right)))))

(mergesort '(3 7 2 3 1 9 8 100))
(mergesort '(1 23 43 431 11 34 -1 3))
