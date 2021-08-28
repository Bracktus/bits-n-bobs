#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list

(define (last-pair items)
  (list (list-ref items (- (length items) 1)))
)

(define (last-pair2 items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items 
        (last-pair2 rest)
    )
  )
)

(last-pair (list 23 72 149 34))
(last-pair2 (list 23 72 149 34))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

;Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order

(define (reverse items)
  (define (do-reverse items reversed)
    (if (null? items)
        reversed
        (do-reverse (cdr items) (cons (car items) reversed))
    )
  )
  (do-reverse items '())
)

(reverse (list 1 2 3 4))

;(do-reverse '(1 2 3 4) '())
;(do-reverse '(2 3 4) '(1))
;(do-reverse '(3 4) '(2 1))
;(do-reverse '(4) '(3 2 1))
;(do-reverse '() '(4 3 2 1))

(define (deep-reverse items)
  (define (do-deep-rev items reversed)
    (cond ((null? items) 
            reversed)

           ((pair? (car items))
           (do-deep-rev (cdr items) (cons (deep-reverse (car items)) reversed)))

           (else 
             (do-deep-rev (cdr items) (cons (car items) reversed)))
    )
  )
  (do-deep-rev items '())
)

(define x 
  (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)

; '((4 3 (2 1)) (2 1))

;Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order.

(define (fringe tree)
  (define (do-fringe tree res)
      (cond ((null? tree)
             res)

            ((not (pair? tree))
             (list tree))

            ((pair? tree)
             (append (do-fringe (car tree) res)
                     (do-fringe (cdr tree) res)))
      )
  )
  (do-fringe tree '())
)

(fringe x)
(newline)
(fringe (list x x))

