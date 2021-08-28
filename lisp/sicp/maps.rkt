#lang racket

(define (reverse items)
    (define (do-reverse items reversed)
      (if (null? items)
          reversed
          (do-reverse (cdr items) (cons (car items) reversed))
      )
    )
    (do-reverse items '())
)

(define (same-parity first . rest)
  (let ((parity (remainder first 2)))

    (define (list-gen items new-items)
      (cond ((null? items) new-items)

             ((= parity (remainder (car items) 2))
              (list-gen (cdr items) (cons (car items) new-items)))

             (else 
              (list-gen (cdr items) new-items))))

    (reverse (list-gen rest '()))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 2 3 4 5 6 7)

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))
  )
)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))
  )
)

(map abs (list -10 2.5 -11.6 17))

(define (scale-list2 items factor)
  (map (lambda (x) (* x factor)) items)
)

(define (square n) (* n n))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))
  )
)

(define (square-list2 items)
  (map square items)
)

(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      #t
      (for-each proc (cdr items)))
)

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88)
)

((lambda (x) (car (cdr (car (cdr (cdr x)))))) '(1 3 (5 7) 9))
((lambda (x) (car (car x))) '((7)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (scale-tree tree factor)
  (cond ((null? tree) '())

        ((not (pair? tree)) (* tree factor))

        (else
          (cons (scale-tree (car tree) factor))
                (scale-tree (cdr tree) factor))
  )
)

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor))
       )
       tree)
)

(define (square-tree tree)
  (cond ((null? tree) '())

        ((not (pair? tree)) (* tree tree))

        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree)))))
)

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree))
       )
       tree)
)

(define (tree-map prod tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map prod sub-tree)
             (prod sub-tree))
       )
       tree)
)
(newline)
(displayln "treemap")
(define my-tree (list (list 1 2) 3 (list 4 5))) 
(tree-map square my-tree) 
(tree-map (lambda (x) (+ x 1)) my-tree) 
(define nil '())

;yh i cheated on this one, no idea why it works...
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;If the sequence is null: nil
;If the element fits the predicate: add it to the result
;Else, move onto the next value in the sequence
(define (filter predicate sequence)
  (cond ((null? sequence) 
         nil)

        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))

        (else  (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
                (enumerate-tree (car tree))
                (enumerate-tree (car tree))))))
  

(define (fib n)
  (define (iter-fib a b count)
    (if (= count n)
        a
        (iter-fib (+ a b) a (+ count 1))
    )
  )
  (iter-fib 0 1 0)
)

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)

(define (even-fibs n)
  (accumulate cons 
              nil 
              (filter even? (map fib (enumerate-interval 0 n))))
)

(even-fibs 10)

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square (map fib (enumerate-interval 0 n))))
)

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence)))
)

(product-of-squares-of-odd-elements 
 (list 1 2 3 4 5))

(define (map-acc p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence)
)

(define (append-acc seq1 seq2)
  (accumulate cons seq1 seq2)
)

(define (length-acc sequence)
  (accumulate (lambda (x y) 
                      (+ 1 y)) 
              0 
              sequence))


(length-acc '(1 2 3 4))

(define (honer-eval x coeffiecient-sequence)
  (accumulate 
    (lambda (this-coeff higher-terms)
            (+ this-coeff (* x higher-terms)))
    0 
    coeffiecient-sequence))

(honer-eval 2 (list 1 3 0 5 0 1))

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

(define (count-leaves-acc t)
  (accumulate (lambda (x y) (+ 1 y)) 
              0 
              (map (lambda (x) (enumerate-interval 0 x)) (fringe t)))
)

(define (count-leaves-acc2 t)
  (accumulate (lambda (x y) (+ 1 y)) 
              0 
              (fringe t))
)
(define tree (list 1 2 (list 3 4) (list 5 (list 6 7)))) 

(count-leaves-acc tree) ;; => 7 
(count-leaves-acc2 tree) ;; => 7 

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))
  )
)

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;(fold-left / 1 (list 1 2 3)) 
;(iter 1 [1 2 3])

;(iter (/ 1 1) [2 3])
;(iter 1 [2 3])

;(iter (/ 1 2) [3])
;(iter 0.5 [3])

;(iter (/ 0.5 3) [])
;(iter 0.1667 [])
;(eq? 0.1667 (/ 1 6)) 

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

;(fold-right / 1 (list 1 2 3))
;(fold-right / 1 [1 2 3])
;(/ 1 (fr / 1 [2 3]))
;(/ 1 (/ 2 (fr / 1 [3])))
;(/ 1 (/ 2 (/ 3 (fr / 1 []))))
;(/ 1 (/ 2 (/ 3 1))
;(/ 1 (/ 2 3))
;(/ 1 0.666)
;(1.5)

(fold-left / 1 (list 1 2 3))
(fold-right / 1 (list 1 2 3))

(fold-left list nil (list 1 2 3))
;(() 1)
;((() 1) 2)
;(((() 1) 2) 3)

(fold-right list nil (list 1 2 3))
;(3 ())
;(2 (3 ()))
;(1 (2 (3 ())))

(define (reverse-left sequence)
  (fold-left 
    (lambda (x y) (cons y x)) 
    nil 
    sequence))

(reverse-left '(1 2 3))

(define (reverse-right sequence)
  (fold-right 
    (lambda (x y) (append y (list x)))
    nil
    sequence))

(reverse-right '(1 2 3))

;maps a proc onto a seq and adds each val to a list in order
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;(cadr a) is (car (cdr a))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (loop d)
    (cond ((> d (sqrt n)) #t)
          ((= 0 (remainder n d)) #f)
          (else (loop (+ d 1)))))
  (loop 2))

(prime? 93)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval
                          1
                          (- i 1))))
                 (enumerate-interval 1 n)))))

(prime-sum-pairs 6)
