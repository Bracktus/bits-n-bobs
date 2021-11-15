#lang sketching

(provide poly+)
(provide poly*)
(provide interpolate)
(provide poly->func)

;-------------PHYSICS------------

(define point-list
  (list (cons 100 0)
        (cons 200 200)
        (cons 300 300)
        (cons 400 103)))

(define accel-list
  (list 8 3 5 4))

(define (bound points accels)
  
  (define (bound-one point accel)
    (cond [(> (cdr point) height)
           (* -1 accel)]
        
          [(< (cdr point) 0)
           (* -1 accel)]
        
          [else accel]))

  (map bound-one points accels))
  
(define (move-points points accels)
  (map
   (λ (point accel) (cons (car point) (+ accel (cdr point))))
   points accels))

;-------------DRAWING------------

(define (setup)
  (smoothing 'smoothed)
  (size 500 500)
  (set-frame-rate! 30))

(define (draw)
  ;(translate (/ width 2) (/ height 2))
  (background 0)
  (set! point-list (move-points point-list accel-list))
  (set! accel-list (bound point-list accel-list))
  (let* ([points point-list]
         [poly (interpolate points)]
         [f (poly->func poly)])
    

    (no-fill)
    (stroke 255)
    (begin-shape)
    (for ([x (in-range 0 500 10)])
      (vertex x (f x)))
    (end-shape)

    (fill 255)
    (for ([point points])
      (circle (car point) (cdr point) 10))))

;------------POLYNOMIAL CALCULATIONS-----------------

; a polynomial is a list of coefficients. '(2 0 3) -> 2 + 0x + 3x^2
;converts a polynomial into a function
(define (poly->func poly)
  
  (define (poly-func poly val)    
    (let loop ([lst poly]
               [power 0]
               [acc 0])
      
      (if (null? lst)
          acc
          (loop (cdr lst)
                (add1 power)
                (+ acc (* (car lst)
                          (expt val power)))))))

  (λ (value) (poly-func poly value)))

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
      
      (define curr-val
        (vector-ref final (+ i j)))
      
      (vector-set! final
                   (+ i j)
                   (+ curr-val (* (vector-ref vec-1 i)
                                  (vector-ref vec-2 j)))))
    
    (vector->list final)))

;returns a n degree polynomial function passing through all the points
(define (interpolate points)
  ;points is a list of xy pairs
  (define poly-list
    (map (λ (point) (get-term point points)) points))

  (foldl poly+ '(0) poly-list))

;returns a term of the lagrange polynomial.
;add them all and you get the whole thing
(define (get-term point points)

  (define (get-subterms x1 x2)
    ;y * (x - xi / xi - xj)
    (let ([a0 (/ (- x2) (- x1 x2))]
          [a1 (/ 1 (- x1 x2))])
      (list a0 a1)))
    
  (let* ([n-points (remove point points)];ignore where xi = xj
         [x (car point)]
         [y (cdr point)]
         [subterms (map (λ (p) (get-subterms x (car p))) n-points)])
    (poly* (list y) (foldl poly* '(1) subterms))))

