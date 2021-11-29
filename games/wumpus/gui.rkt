#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 1000]
                   [height 1000]))

(define wumpus-canvas%
  
  (class canvas%

    (define (draw-grid w h)
      (define dc (send this get-dc))
      (for ([x (in-range 0 w (/ w 5))]
            [y (in-range 0 h (/ h 5))])
      
        ;horizontal line
        (send dc draw-line
              x 0
              x h)
        ;vertical line
        (send dc draw-line
              0 y
              w y)))

    (define (move key)
      (let ([dc (send this get-dc)]
            [frame (send this get-parent)]
            [w (send frame get-width)]
            [h (send frame get-height)])
        
        (case key
          [(right) (println "hello")]
          [(left) (draw-grid w h)])))
    
    (define/override (on-char event)
      (move (send event get-key-code)))

    (super-new)))

(new wumpus-canvas% [parent frame])

(send frame show #t)
    
    
    
  