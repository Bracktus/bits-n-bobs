#lang sketching
(require racket/list)

(class Player Object
  (init-field x y x-vel y-vel attached)
  (super-new)
  
  ;direction is whether we're rotating clockwise or anti clockwise around the ball.
  ;either -1 or 1
  (define-values (direction) (values 1))

  ;saving the previous values
  (define-values (trail-len) (values 30))
  (define-values (trail-x trail-y) (values (make-vector trail-len)
                                           (make-vector trail-len)))

  (define/public (render)
    (no-fill)
    (stroke 255)
    (render-trail)
    (unless (equal? 'empty attached)
      ;(test-direction)
      (render-attachment)))
  
  (define/public (update!)

    ;apply velocity
    (set! x (+ x x-vel))
    (set! y (+ y y-vel))

    ;gradual deacceleration
    (set! x-vel (* x-vel 0.999))
    (set! y-vel (* y-vel 0.999))
    
    ;bounce the ball of the walls
    (when (or (> x width) (< x 0))
      (set! x-vel (- x-vel)))

    (when (or (> y height) (< y 0))
      (set! y-vel (- y-vel)))

    ;when attached, set velocity to the tangent vector of the player and the ball
    (unless (equal? attached 'empty)
      (let-values ([(neo-x-vel neo-y-vel) (perp 10)])
        (set! x-vel (* direction neo-x-vel))
        (set! y-vel (* direction neo-y-vel))
        
        ;cap velocity
        (let* ([vel-mag (mag x-vel y-vel)]
               [vel-norm-x (/ x-vel vel-mag)]
               [vel-norm-y (/ y-vel vel-mag)])
          (when (> vel-mag 40)
            (set! x-vel (* vel-norm-x 40))
            (set! y-vel (* vel-norm-y 40))))))

    ;store previous positions to render a trail
    (let ([trail-idx (remainder frame-count trail-len)])
      (vector-set! trail-x trail-idx x)
      (vector-set! trail-y trail-idx y)))

  (define (get-direction)
    ;Decides if we should be rotating clockwise or anti-clockwise
    ;https://stackoverflow.com/questions/13221873/determining-if-one-2d-vector-is-to-the-right-or-left-of-another
    (let*  ([v1-x (- (attached.get-x) x)]
            [v1-y (- (attached.get-y) y)]
            [det (- (* v1-x y-vel)
                    (* v1-y x-vel))])
     (if (> det 0) -1 1)))
        
  (define/public (attach! point-lst)
    (set! attached (nearest point-lst))
    (set! direction (get-direction)))
  
  (define/public (detach!)
    (set! attached 'empty)
    ;if we detach and find ourselves out of bounds teleport back in
    (cond [(> x width)  (set! x 10)]
          [(< x 0)      (set! x (- width 10))])
    (cond [(> y height) (set! y 10)]
          [(< y 0)      (set! y (- height 10))]))
  
  (define (render-trail)
    ;https://soegaard.github.io/sketching/Examples.html
    ;3.3.7 Storing Input
    (for ([i trail-len])
      (define start (modulo frame-count trail-len))
      (define idx (modulo (+ start 1 i) trail-len))
      (circle (trail-x.ref idx)
              (trail-y.ref idx)
              (/ i 2))))
  
  (define (perp mult)
    ;returns the tangent vector of Player and Ball
    (let* ([x-diff (- x (attached.get-x))]
           [y-diff (- y (attached.get-y))]

           [d (dist x y (attached.get-x) (attached.get-y))]
           ;Have to recompute distance every frame because adding perp vector is inprecise and slowly increases radius.
           ;Saving it on attach produces a cool effect where the ball speeds up quickly though.
           [x-norm (/ x-diff d)]
           [y-norm (/ y-diff d)]
           [x (* mult x-norm)]
           [y (* mult y-norm)])

     (values (- y) x)))

  (define (nearest point-lst)
    (argmin (λ (p) (dist x y (p.get-x) (p.get-y)))
            point-lst))
        
  (define/public (render-attachment)
    (let* ([att-x (attached.get-x)]
           [att-y (attached.get-y)]
           [att-r (attached.get-r)]
           
           [a (atan2 (- y att-y) (- x att-x))]          
           [x1 (+ att-x (* (/ att-r 2) (cos a)))]
           [y1 (+ att-y (* (/ att-r 2) (sin a)))])
      (line x1 y1 x y))))
  
(class Ball Object
  (init-field x y r)
  (super-new)
  (define/public (get-x) x)
  (define/public (get-y) y)
  (define/public (get-r) r)

  (define/public (render)
    (stroke 255 0 0)
    (circle x y r)))

(define me
  (make-object Player 300 300 5 3 'empty))

(define p1
  (make-object Ball 230 300 20))
(define p2
  (make-object Ball 200 200 20))
(define p3
  (make-object Ball 350 350 20))
(define p4
  (make-object Ball 230 350 20))

(define p-lst
  (list p1 p2 p3 p4))

(define (setup)
  (size 600 600)
  (background 0)
  (set-frame-rate! 90))

(define (draw)
  (background 0)
  (me.update!)
  (me.render)
  (for ([p p-lst])
    (p.render)))

(define (on-key-pressed)
  (me.attach! p-lst))

(define (on-key-released)
  (me.detach!))

