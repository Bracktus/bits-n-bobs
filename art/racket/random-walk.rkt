#lang sketching

(define start-point (list 200 200))
(define radius 30)
(define old-ang (/ pi 2))

(define (setup)
  (background 0)
  (size 400 400))

(define (draw)
   (stroke 255)
   (no-fill)
   (let* ([ang (random (* 2 pi))]
          [old-x (car start-point)]
          [old-y (cadr start-point)]
          [new-x (+ old-x (* radius (cos ang)))]
          [new-y (+ old-y (* radius (sin ang)))])
     
     (line old-x old-y new-x new-y)
     
     (cond [(< new-x 100)
            (set! start-point (list 300 new-y))]
            
            [(> new-x 300)
             (set! start-point (list 100 new-y))]

            [(< new-y 100)
             (set! start-point (list new-x 300))]

            [(> new-y 300)
             (set! start-point (list new-x 100))]
            
            [else
             (set! start-point (list new-x new-y))])))

  

