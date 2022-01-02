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

  (define/public (get-x) x)
  (define/public (get-y) y)
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
      (let-values ([(neo-x-vel neo-y-vel) (perp 20)])
        (set! x-vel (* direction neo-x-vel))
        (set! y-vel (* direction neo-y-vel))
        
        ;cap velocity
        (let* ([vel-mag (mag x-vel y-vel)]
               [vel-norm-x (/ x-vel vel-mag)]
               [vel-norm-y (/ y-vel vel-mag)])
          (when (> vel-mag 60)
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

(class Ball-List Object
   (init-field n [ball-lst (list)])
   (super-new)

   (define/public (init-lst)
     (set! ball-lst
       (for/list ([i (in-range n)])
         (define x (* width (random)))
         (define y (* height (random)))
         (make-object Ball x y 20))))

   (define/public (get)
     ball-lst)

   (define/public (render)
     (for ([ball ball-lst])
        (ball.render))))

(class Target Object
 (super-new)
 (define-values (x y r) (values (* width (random)) 
                                (* height (random))
                                100))

 (define-values (times-teleported) 0)

 (define/public (get-score)
    times-teleported)

 (define/public (update! player-x player-y)
    (when (inside? player-x player-y)
      (set! times-teleported (add1 times-teleported))
      (teleport!)))
        
 (define (inside? x2 y2)
    (let ([x-diff^2 (sq (- x2 x))]
          [y-diff^2 (sq (- y2 y))])
      (< (+ x-diff^2 y-diff^2) (sq (/ r 2)))))

 (define (teleport!)
    (set! x (* width  (random)))
    (set! y (* height (random))))

 (define/public (render)
    (stroke 0 0 255)
    (circle x y r)))

(class Timer Object
  (init-field end)
  (super-new)
  (define-values (start) 0)

  (define/public (reset!)
    (set! start (floor (/ (millis) 1000))))
  
  (define/public (current)
    (- (floor (/ (millis) 1000)) start))

  (define/public (over?)
    (> (current) end))

  (define/public (render)
    (define secs 
      (max 0 (- end (current))))

    (fill 0 0 255)
    (text (number->string secs) 10 10)))

(define me
  (make-object Player 300 300 5 3 'empty))
(define them
  (make-object Ball-List 7))
(define targ
  (make-object Target))
(define timer
  (make-object Timer 30))

(define (reset-game!)
  (timer.reset!)
  
  (set! them
        (make-object Ball-List 7))
  (them.init-lst)
  (set! targ
        (make-object Target)))
  
(define (setup)
  (fullscreen)
  (background 0)
  (them.init-lst)
  (set-frame-rate! 90))

(define (draw)
  (background 0)
  (no-cursor)
  
  (cond [(timer.over?)
         (fill 0 0 255)
         (text "Time over!" (/ width 2) (/ height 2))
         (text (format "Score: ~a" (targ.get-score))
               (/ width 2) (+ 20 (/ height 2)))
         (text "Press any button to restart" (/ width 2) (+ 40 (/ height 2)))]

        [else
          (timer.render)
          (me.update!)
          (targ.update! (me.get-x) (me.get-y))
          (me.render)
          (targ.render)
          (them.render)]))

(define (on-key-pressed)
  (when (timer.over?)
    (reset-game!))
  (me.attach! (them.get)))

(define (on-key-released)
  (unless (timer.over?)
    (me.detach!)))

