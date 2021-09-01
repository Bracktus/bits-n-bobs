#lang sketching

;Verlet Integration
;Have a bunch of points
;Connect the points with sticks that constrain the points

;We can assign each dot a unique identifier and insert it into a hash table
;A dot would look like this: '((x1 . y1) (x2 . y2))
;If the dot's ID is negative, then the dot is pinned.

;We can define a stick as a connection between 2 dots.
;A stick would look like this '(len point1-id point2-id)
;where len is the inital distance between point1 and point2

(define gravity 0.9)
(define bounce-factor 0.99)
(define friction 0.99)

(define dot-hash
  (make-hash))

(define stick-list
  '())

(define (add-dot id curr-x curr-y old-x old-y)
  (hash-set! dot-hash
             id
             (list (cons curr-x curr-y) (cons old-x old-y))))

(define (add-stick dot1-id dot2-id)
  (set! stick-list (cons (list (distance dot1-id dot2-id)
                               dot1-id
                               dot2-id)
                         stick-list)))

(define (toggle-fixed id)
  ;causes race condition :(
  (let ([copy (hash-ref dot-hash id)])
    (hash-remove! dot-hash id)
    (hash-set! dot-hash (* -1 id) copy)))

(define (distance dot1-id dot2-id)
  (let* ([p1 (hash-ref dot-hash dot1-id)]
         [p2 (hash-ref dot-hash dot2-id)]
         [dx (- (caar p1) (caar p2))]
         [dy (- (cdar p1) (cdar p2))])
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (setup)
  (set-frame-rate! 30)
  (size 800 1000)
  
  (add-dot 1   100 100 95 95)
  (map (λ (n) (add-dot n (+ (* n 30) 80) 100 (+ (* n 30) 80) 100))
       '(-2 3 4 5 6 7))
    
  (add-stick  1 -2)
  (map (λ (n) (add-stick n (add1 n))) '(-2 3 4 5 6)))

;We can wrap render-sticks and constrain-dots in a for loop to reduce the bounciness
;I like the effect so I'll leave it out
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

(define (update-dot id)
  (when (positive? id)
    (let* ([dot    (hash-ref dot-hash id)]
           [curr-x (caar dot)]
           [curr-y (cdar dot)]
           [old-x  (caadr dot)]
           [old-y  (cdadr dot)]

           [velocity-x (* friction (- curr-x
                                      old-x))]
           [velocity-y (* friction (- curr-y
                                      old-y))]

           [new-x (+ curr-x velocity-x)]
           [new-y (+ curr-y velocity-y)])
    
      (hash-set! dot-hash id (list (cons new-x (+ gravity new-y))
                                   (cons curr-x curr-y))))))

(define (constrain-dots id)
  (when (positive? id)
    (let* ([dot    (hash-ref dot-hash id)]
           [curr-x (caar dot)]
           [curr-y (cdar dot)]
           [old-x  (caadr dot)]
           [old-y  (cdadr dot)]

           [velocity-x (* friction (- curr-x
                                      old-x))]
           [velocity-y (* friction (- curr-y
                                      old-y))]
           [new-x (+ curr-x velocity-x)]
           [new-y (+ curr-y velocity-y)])
    
      (when (or (> new-x width) (< new-x 0) (> new-y height) (< new-y 0))
        (hash-set! dot-hash id (cond [(> new-x width) (list (cons width new-y)
                                                            (cons (+ width (* bounce-factor velocity-x)) curr-y))]
                                 
                                     [(< new-x 0) (list (cons 0 (+ gravity new-y))
                                                        (cons (* bounce-factor velocity-x) curr-y))]

                                     [(> new-y height) (list (cons new-x height)
                                                             (cons curr-x (+ height (* bounce-factor velocity-y))))]
                                 
                                     [(< new-y 0) (list (cons new-x 0)
                                                        (cons curr-x (* bounce-factor velocity-y)))]))))))

(define (update-stick stick)
  (let* ([id-1 (cadr stick)]
         [id-2 (caddr stick)]
         [p1 (hash-ref dot-hash id-1)]
         [p2 (hash-ref dot-hash id-2)]
         [dx (- (caar p1) (caar p2))]
         [dy (- (cdar p1) (cdar p2))]

         [dist (sqrt (+ (* dx dx) (* dy dy)))]
         [len  (car stick)]
         [diff (- len dist)]
         [percent (/ diff dist 2)]
         [off-x (* dx percent)]
         [off-y (* dy percent)])
    
    (when (positive? id-1)
      (add-dot id-1
               (+ (caar p1) off-x)
               (+ (cdar p1) off-y)
               (caadr p1)
               (cdadr p1)))
    
    (when (positive? id-2)
      (add-dot id-2
               (- (caar p2) off-x)
               (- (cdar p2) off-y)
               (caadr p2)
               (cdadr p2)))))
         
(define (render-dot dot diam)
  (let* ([x (caar dot)]
         [y (cdar dot)])
    (fill 255)
    (circle x y diam)))

(define (render-stick stick)
  (let* ([id-1 (cadr stick)]
         [id-2 (caddr stick)]
         
         [p1 (hash-ref dot-hash id-1)]
         [p2 (hash-ref dot-hash id-2)]
         
         [p1-x (caar p1)]
         [p1-y (cdar p1)]
         [p2-x (caar p2)]
         [p2-y (cdar p2)])
    (stroke 255)
    (line p1-x p1-y p2-x p2-y)))
    
