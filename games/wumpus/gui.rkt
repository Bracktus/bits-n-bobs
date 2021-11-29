#lang racket/gui

;---brushes-----
(define explored-brush
  (new brush% [color (make-object color% 0 255 13)]))

(define hidden-brush
  (new brush% [color (make-object color% 133 124 122)]))

;---game stuff---
(require "game.rkt")

(define grd (create-grid))
(define plr (player (cons 0 0) 5))
(populate-grid! 2 2 grd plr)

;------------------
(define wumpus-canvas%
  (class canvas%
    
    (define (on-key-input key)
      (let* ([dc (send this get-dc)]
             [parent (send this get-parent)]
             [w (send parent get-width)]
             [h (send parent get-height)])
        
        (when (member key '(left right up down))
          (game-move plr key grd)
          (game-draw plr w h dc))

        (when (equal? key 'space)
          (game-draw-reveal grd w h dc))))
    
    (define/override (on-char event)
      (on-key-input (send event get-key-code)))

    (super-new)))

(define frame (new frame%
                   [label "Example"]
                   [width 500]
                   [height 500]))

(define panel (new vertical-panel% [parent frame]))
(define wumpus-canvas (new wumpus-canvas%
                           [parent panel]
                           [paint-callback
                            (λ (canvas dc)
                              (game-draw plr
                                         (send frame get-width)
                                         (send frame get-height)
                                         dc))]))

(define description-text
  (new message%
       [parent panel]
       [horiz-margin (/ (send frame get-height) 5)]
       [label ""]))
  
(send frame show #t)

(define (game-move player direction grid)
  (move-player player direction grid) ;moves the player
  
  (define description
    (string-join (adjacent-info grid (player-position player))
                 "\n"))
  
  (send description-text set-label description)
  
  (unless (status player grid) ;when the player is dead
    (lose)))

(define (lose)
  (send description-text set-label "You Died"))

(define (game-draw-reveal grid w h dc)
  (println "2"))

(define (game-draw player w h dc)
  (send dc clear)
  (draw-grid grd w h dc)
  (draw-player player w h dc))

(define (draw-box x y w h brush dc)
  (let ([rect-w (/ w 5)]
        [rect-h (/ h 5)])
    (send dc set-brush brush)
    (send dc draw-rectangle
          (* (/ w 5) x)
          (* (/ h 5) y)
          rect-w
          rect-h)))

(define (draw-grid grid w h dc)
  ;drawing the basic grid, no monsters no humans
  (for ([i (cart-prod 5 5)])
    (draw-box (car i) (cdr i) w h
              (if (tile-explored (hash-ref grid i))
                  explored-brush
                  hidden-brush)
              dc)))
        
(define (draw-player player w h dc)
  (let* ([pos (player-position player)]
         [x (car pos)]
         [y (cdr pos)]
         [new-x (+ (/ (/ w 5) 2) (* (/ w 5) x))]
         [new-y (+ (/ (/ h 5) 2) (* (/ h 5) y))])
    (send dc set-brush "blue" 'solid)
    (send dc draw-ellipse new-x new-y 10 10)))
