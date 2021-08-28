#lang sketching
(define total-frames 60)
(define counter 0)

(define (setup)
  (set-frame-rate! 240)
  (size 400 400))

(define (render percent)
  (background 0)
  (define angle (* percent 2 pi))
  (translate (/ width 2) (/ height 2))
  (rect-mode 'center)
  (no-fill)
  (stroke 255)
  (stroke-weight 2)
  (rotate angle)
  (square 0 0 100))

(define (draw)
  (render (/ counter total-frames))
  (set! counter (+ counter 1)))



  
  
  