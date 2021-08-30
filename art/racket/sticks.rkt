#lang sketching


;Verlet Integration
;Have a bunch of points
;Connect the points with sticks that constrain the points

(struct dot (x y old-x old-y))

(define dot-list
  (list (dot 100 100 95 95)))
  
(define (setup)
  (size 800 800))

(define (draw)
  (background 0)
  (set! dot-list (update-dots dot-list))
  (render-dots dot-list))

(define (update-dots dots)
;https://www.youtube.com/watch?v=3HjO_RGIjCU 9:50
  (define (update idot)
    (let* ([velocity-x (- (dot-x idot)
                          (dot-old-x idot))]

           [velocity-y (- (dot-y idot)
                          (dot-old-y idot))])
      
      (dot (+ (dot-x idot) velocity-x)
           (+ (dot-y idot) velocity-y)
           (dot-x idot)
           (dot-y idot))))
  
    (map update dots))

(define (render-dots dots)
  (no-fill)
  (stroke 255)
  (for ([idot dots])
    (circle (dot-x idot)
            (dot-y idot)
            5)))
  
  
           
    
           
           
  
  