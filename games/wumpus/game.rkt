#lang racket
;grid is a hash table.
;it's keys are position pairs, it's keys are a tile

(define width 5)
(define height 5)

(struct player
  (position arrows)
  #:transparent
  #:mutable)

(struct tile
  (explored smell breeze flapping wumpus pit bat)
  #:transparent
  #:mutable)

(define (cart-prod x y)
  (for*/list ([x (in-range 5)]
              [y (in-range 5)])
    (cons x y)))

(define (create-grid)
  (let ([table (make-hash)])
    (for-each (λ (val) (hash-set! table
                                  val
                                  (tile #f #f #f #f #f #f #f)))
              (cart-prod 5 5))
    table))

(define (teleport player)
  ;moves the user to a random place
  (let* ([rand-x (random 0 width)]
         [rand-y (random 0 height)]
         [pos (cons rand-x rand-y)])
    (set-player-position! pos)))

(define (get-adjacent pos)
  ;returns a list of adjacent positions
  (let ([x (car pos)]
        [y (cdr pos)])
    
    (filter (λ (v) (not (void? v)))
            (list
             (when (< (add1 x) width)
               (cons (add1 x) y))
             
             (when (> (sub1 x) -1)
               (cons (sub1 x) y))
             
             (when (< (add1 y) height)
               (cons x (add1 y)))
             
             (when (> (sub1 y) -1)
               (cons x (sub1 y)))))))

(define (sub-arrow! player)
  ;removes an arrow from the player
  (set-player-arrows! player
                      (sub1 (player-arrows player))))

(define (status player grid)
  ;return false if player is dead
  ;return true if player is alive
  (let* ([pos (player-position player)]
         [curr-tile (hash-ref grid pos)])
    
    (set-tile-explored! curr-tile #t)
    
    (cond [(tile-wumpus curr-tile) #f]
          [(tile-pit curr-tile) #f]
          [else #t])))

(define (adjacent-info grid pos)
  ;returns a list of descriptions 
  (define (describe-tile tile)
       (filter (λ (v) (not (void? v)))
               (list
                (when (tile-smell tile)
                  "You smell the wumpus nearby")
             
                (when (tile-breeze tile)
                  "You feel a breeze nearby")
             
                (when (tile-flapping tile)
                  "You hear some flapping nearby"))))

   (let* ([adj-pos (get-adjacent pos)]
          [adj-tiles (map (λ (p) 
                             (hash-ref grid pos)) 
                          adj-pos)])
     (remove-duplicates (flatten 
                          (map describe-tile adj-tiles)))))


(define (move-player player direction grid)
  ;moves a player up, down, left or right.
  ;if the movement is impossible, then do nothing.
  (let* ([pos (player-position player)]
         [x (car pos)]
         [y (cdr pos)])
    
   (define new-pos
     (case direction
       [(left) (cons (sub1 x) y)]
       [(right) (cons (add1 x) y)]
       [(up) (cons x (sub1 y))]
       [(down) (cons x (add1 y))]))

   (when (member new-pos (get-adjacent pos))
     (set-player-position! player new-pos))))

(define (shoot player direction grid)
  ;shoots an arrow.
  (let* ([pos (player-position player)]
         [x (car pos)]
         [y (cdr pos)])
    
   (define arrow-pos
     (case direction
       [(left) (cons (sub1 x) y)]
       [(right) (cons (add1 x) y)]
       [(up) (cons x (sub1 y))]
       [(down) (cons x (add1 y))]))

   ;if the shot is valid
   (if (member arrow-pos (get-adjacent pos))
        
       ;Then remove an arrow from the player
       (begin
         (sub-arrow! player) ;arrows -= 1
         ;and return true if the wumpus was on that tile
         ;otherwise return false
         (tile-wumpus (hash-ref grid pos)))
        
       ;invalid shot, return false
       #f)))
          

(define (apply-n proc n lst)
  ;applies a function to a list n times
  (let loop ([count 0]
             [lst lst])
    (if (= count n)
        lst
        (loop (add1 count)
              (proc lst)))))

;(define grd (create-grid))
;(define plr (player (cons 0 0) 5))

(define (populate-grid! n-pits n-bats grid player)
  ;populates the grid with the player, bats, pits and a wumpus

  (define (place-player lst)
    ;places a player on the grid
    ;returns a list of positions without:
    ;the player's tile,
    ;the tiles adjacent to player
    (let* ([pos (car lst)]
           [adj-pos (get-adjacent pos)]
           [player-tile (hash-ref grid pos)])
      
      (set-player-position! player pos)
      (set-tile-explored! player-tile #t)
      (remove* (cons pos adj-pos) lst)))

  ;candidate for a macro
  (define (place-wumpus lst)
    (let* ([pos (car lst)]
           [adj-pos (get-adjacent pos)]
           [wump-tile (hash-ref grid pos)]
           [smell-tiles (map (λ (p) (hash-ref grid p))
                             adj-pos)])
      
      (set-tile-wumpus! wump-tile #t)
      (for-each (λ (t) (set-tile-smell! t #t))
                smell-tiles)
      (cdr lst)))

  (define (place-bat lst)
    (let* ([pos (car lst)]
           [adj-pos (get-adjacent pos)]
           [bat-tile (hash-ref grid pos)]
           [flap-tiles (map (λ (p) (hash-ref grid p))
                            adj-pos)])
      
      (set-tile-bat! bat-tile #t)
      (for-each (λ (t) (set-tile-flapping! t #t))
                flap-tiles)
      (cdr lst)))

  (define (place-pit lst)
    (let* ([pos (car lst)]
           [adj-pos (get-adjacent pos)]
           [pit-tile (hash-ref grid pos)]
           [breeze-tiles (map (λ (p) (hash-ref grid p))
                              adj-pos)])
      
      (set-tile-pit! pit-tile #t)
      (for-each (λ (t) (set-tile-breeze! t #t))
                breeze-tiles)
      (cdr lst)))
  
  (let ([positions (shuffle (cart-prod width height))])
    ;player + adjacent spaces can't be in danger
    (place-bat (place-pit (place-wumpus (place-player positions))))))

(provide (all-defined-out))

