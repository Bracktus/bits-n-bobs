#lang sketching

(define num-points 100)
(define factor 0)

(define (setup)
  (cursor 'blank)
  (set-frame-rate! 60)
  (size 800 800))

(define (draw)
  (set! num-points (round (/ mouse-x 6)))
  (set! factor (+ factor 0.05))
  (background 0)
  ;(define delta (/ (* 2 pi) total))
  (translate (/ width 2) (/ height 2))
  (define radius (/ width 4))

  ;draw a circle
  (stroke 255)
  (no-fill)
  (circle 0 0 (* radius 2))

  ;for every value, plot it's point on a circle
  (for ([i (in-inclusive-range 1 num-points)])
    (let* ([angle (* (/ i num-points) (* 2 pi))]
           [x (* radius (cos angle))]
           [y (* radius (sin angle))])
      (fill 255)
      (circle x y 16)))
  
  ;for every value, times it by every value and mod it by num-points
  (for ([i (in-range num-points)])
    (let* ([prod (* i factor)]
           [res (real-mod prod num-points)])
      ;create a link between the value and the result
      (call-with-values (lambda () (get-vec radius i res))
                        (lambda (x1 y1 x2 y2) (line x1 y1 x2 y2)))))) 

(define (get-vec radius from to)
  (let* ([from-angle (* (/ from num-points) (* 2 pi))]
         [from-x (* radius (cos from-angle))]
         [from-y (* radius (sin from-angle))]
         
         [to-angle (* (/ to num-points) (* 2 pi))]
         [to-x (* radius (cos to-angle))]
         [to-y (* radius (sin to-angle))])
    (values from-x from-y
            to-x   to-y)))

(define (real-mod x y)
  (- x (* y (floor (/ x y)))))

(define (round-2dp x)
  (/ (floor (* 100 x)) 100))
         

