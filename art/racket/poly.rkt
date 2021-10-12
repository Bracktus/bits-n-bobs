#lang racket

(provide poly+)
(provide poly*)
(provide interpolate)

(define points (list (cons 1 1) (cons 2 4) (cons 7 9)))
; a polynomial is a list of coefficients. '(2 0 3) -> 2 + 0x + 3x^2

;converts a polynomial into a function
;(define (poly->func poly))

;adds 2 polynomials
(define (poly+ poly-1 poly-2)
   (let loop ([p1 poly-1]
              [p2 poly-2]
              [acc '()])
    (cond
      [(and (null? p1) (null? p2)) (reverse acc)]
      [(null? p1) (append acc p2)]
      [(null? p2) (append acc p1)]
      [else (loop (cdr p1)
                  (cdr p2)
                  (cons (+ (car p1) (car p2)) acc))])))

;multiply 2 polynomials
(define (poly* poly-1 poly-2)
  
  (let* ([poly-len-1 (length poly-1)]
         [poly-len-2 (length poly-2)]
         [vec-1 (list->vector poly-1)]
         [vec-2 (list->vector poly-2)]
         [final (make-vector (sub1 (+ poly-len-1 poly-len-2)) 0)])
        ;initalise a vector of length m + n - 1
        ;where m = poly-len-1...

    (for* ([i (in-range poly-len-1)]
           [j (in-range poly-len-2)])
      
      (define curr-val (vector-ref final (+ i j)))
      
        (vector-set! final (+ i j) (+ curr-val (* (vector-ref vec-1 i)
                                                  (vector-ref vec-2 j)))))
    
    (vector->list final)))

;returns a n degree polynomial function passing through all the points
(define (interpolate points)
  ;points is a list of xy pairs
  (define poly-list
    (map (λ (point) (get-term point points)) points))

  
  (displayln poly-list)
  (foldl poly+ '(0) poly-list))

(define (get-term point points)

  (define (get-subterms x1 x2)
    ;y * (x - xi / xi - xj)
    (let ([a0 (/ (- x2) (- x1 x2))]
          [a1 (/ 1 (- x1 x2))])
      ;(displayln (list a0 a1))
      (list a0 a1)))
    
  (let* ([n-points (remove point points)];ignore where xi = xj
         [x (car point)]
         [y (cdr point)]
         [subterms (map (λ (p) (get-subterms x (car p))) n-points)])
    (poly* (list y) (foldl poly* '(1) subterms))))


