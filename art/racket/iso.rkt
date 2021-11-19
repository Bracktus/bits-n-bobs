#lang sketching

(require math/matrix)

;---------------colours--------------
(define yellow
  (color 255 204 0))

;---------------maths----------------
;https://en.wikipedia.org/wiki/Isometric_projection#Mathematics

(define (rot-around origin point θ)

  (define x
    (vector-ref origin 0))

  (define y
    (vector-ref origin 1))
    
  ;todo think of some better names 
  (define idx-02
    (+ (* (- x) (cos θ))
       (* y (sin θ))
       x))

  (define idx-12
    (- (* (- x) (sin θ))
       (* y (cos θ))
       y))

 ;what this does is it moves the coordinate system to origin
 ;rotates the point
 ;moves back to original position

 (define rot-mat
   ;rotation matrix around a point
   (matrix [[(cos θ)  (- (sin θ)) idx-02]
            [(sin θ)  (cos θ)     idx-12]
            [0        0           1]]))

 (define fin-mat
    ;apply our rotation matrix to our point
    (matrix* rot-mat (vector->matrix 3 1 point)))

 (matrix->vector fin-mat))
 
(define (ortho point)
  ;gets the x y values from a point in iso space
  (values (vector-ref point 0)
          (vector-ref point 1)))

(define (iso point)
  ;converts a point in 3d to a point in iso space
  
  (define (mat*scal mat scal)
    ;matrix scalar multiplication
    (matrix-map (λ (v) (* scal v)) mat))
  
  (define iso-m
    ;maps a 3d point to it's value in iso space
    (let ([s3 (sqrt 3)]
          [s2 (sqrt 2)])
      
      (mat*scal (matrix [[s3 0     (- s3)]
                         [1  2      1]
                         [s2 (- s2) s2]])
                (/ 1 (sqrt 6)))))
  
  (define fin-mat
    ;apply our mapping matrix to our point
    (matrix* iso-m (vector->matrix 3 1 point)))
  
  (matrix->vector fin-mat))

;-------------shapes---------------------

(define (iso-line p1 p2)
  ;draw a edge of a cuboid
  (let-values ([(iso-x1 iso-y1) (ortho p1)]
               [(iso-x2 iso-y2) (ortho p2)])
    (stroke yellow)
    (line iso-x1 iso-y1 iso-x2 iso-y2))) 
      
(define (iso-face p1 p2 p3 p4)
  ;draw a face of a cuboid
  (let-values ([(iso-x1 iso-y1) (ortho p1)]
               [(iso-x2 iso-y2) (ortho p2)]
               [(iso-x3 iso-y3) (ortho p3)]
               [(iso-x4 iso-y4) (ortho p4)])
    
    (begin-shape)
    (vertex iso-x1 iso-y1)
    (vertex iso-x2 iso-y2)
    (vertex iso-x3 iso-y3)
    (vertex iso-x4 iso-y4)
    (end-shape 'close)))

(define (cuboid x y z wid hig dep style)
  ;https://excalidraw.com/#json=Ah-neZtQgizt8Dq-ruEFR,aYpTJ47Sv3YkWp3E_ZWLVA
  ;above is a diagram labelling all verticies
  
  (let ([a (iso (vector (- x wid) (+ y hig) (+ z dep)))]
        [b (iso (vector (- x wid) (+ y hig) (- z dep)))]
        [c (iso (vector (+ x wid) (+ y hig) (- z dep)))]
        [d (iso (vector (+ x wid) (+ y hig) (+ z dep)))]
        [e (iso (vector (- x wid) (- y hig) (+ z dep)))]
        [f (iso (vector (+ x wid) (- y hig) (+ z dep)))]
        [g (iso (vector (+ x wid) (- y hig) (- z dep)))]
        [h (iso (vector (- x wid) (- y hig) (- z dep)))])

   (cond [(equal? style 'wireframe)
          ;the cuboid is made of edges
          (iso-line a b)
          (iso-line a d)
          (iso-line a e)
          (iso-line c d)
          (iso-line c g)
          (iso-line d f)
          (iso-line e f)
          (iso-line f g)
          (iso-line e h)
          (iso-line g h)
          (iso-line b h)
          (iso-line b c)]

         [(equal? style 'opaque)
          ;the cuboid is made of faces
          (iso-face a e f d)
          (iso-face c d f g)
          (iso-face e h g f)])))

;--------------drawing------------------

(define (setup)
  (fullscreen))

(define z 0)
(define (draw)
  (background 0)
  
  (translate (/ width  2)
             (/ height 2))
  
  (fill yellow)
  (cuboid (* 10 (sin z)) 0 0 30 30 30 'opaque)
  (fill (color 255 175 26))
  (cuboid 0 0 100 30 30 30 'opaque)
  (cuboid 100 0 0 30 30 30 'opaque)
  (set! z (add1 z)))
  
;-------------main----------------------

;to make our sketch let's first make a main cube

;then on one side of the cube lets make some cuboids on
;the surface of our main cube

;next, we rotate these cuboids 90 degrees.
;now reflect on the x,y,z axis to get 6 lines of symmetry





    

    
    
