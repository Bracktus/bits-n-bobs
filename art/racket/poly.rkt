#lang racket

; a polynomial is a list of coefficients. '(2 0 3) -> 2 + 0x + 3x^2

;converts a polynomial into a function
(define (poly->func poly))

;adds 2 polynomials
(define (poly+ poly-1 poly-2))

;multiply 2 polynomials
(define (poly* poly-1 poly-2)
  
  (let ([poly-len-1 (length poly-1)]
        [poly-len-2 (length poly-2)]
        [(list->vector lst)]
        [(list->vector lst)])
    
    (define final
      (make-vector (+ poly-len-1 poly-len-2) 0))
  
  (for* ([i (in-range poly-len-1)]
         [j (in-range poly-len-1)])

    (vector-set! (+ i j) (* 
    
;returns a n degree polynomial function passing through all the points
(define (interpolate points)
  ;points is a list of xy pairs
  (when (null? points)
    (error "must provide at least 1 point")))

(define (get-term point points)
  
  (define (get-subterms x-1 x-2)
    ;y * (x - xi / xi - xj)
    (let ([a0 (/ (- x2) (- x1 x2))]
          [a1 (/ 1 (- x1 x2))])
      (list a0 a1)))
    
    
  (let* ([points (filter (λ (p) (= point p)) points)]
         [x (car point)]
         [subterms (map (λ (p) (get-subterms x (car p)) points))]) ;ignore where xi = xj

    (foldr poly* '(1) subterms)))
  

(define (draw)
  (background 0)
  ;update the dots
  (hash-for-each dot-hash (λ (key val) (update-dot key)))
  ;update sticks
  (for ([stick stick-list])
    (update-stick stick))
  ;constrain the points
  (hash-for-each dot-hash (λ (key val) (constrain-dots key)))
  ;render sticks
  (for ([stick stick-list])
    (render-stick stick))
  ;render the dots
  (hash-for-each dot-hash (λ (key val) (render-dot val 5))))

