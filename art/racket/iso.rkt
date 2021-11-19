#lang sketching

(require math/matrix)

; define points in 3D space
; apply isometric view
; but do it 3 times each on a different axis.

;---------------colours--------------
(define yellow
  (color 255 204 0))

;---------------maths----------------
(define (mat*scal mat scal)
  ;not sure why this isn't a builtin
  (matrix-map (λ (v) (* scal v)) mat))

(define (ortho point)
  ;gets the x y values from point in 3D space
  (values (vector-ref point 0)
          (vector-ref point 1)))

(define (iso point)
  ;https://en.wikipedia.org/wiki/Isometric_projection#Mathematics
  (define iso-m
    (let ([s3 (sqrt 3)]
          [s2 (sqrt 2)])
      
      (mat*scal (matrix [[s3 0 (- s3)]
                        [1 2 1]
                        [s2 (- s2) s2]])
                (/ 1 (sqrt 6)))))

  ; a point is just a column vector
  (define fin-mat
    (matrix* iso-m (vector->matrix 3 1 point)))
  
  (matrix->vector fin-mat))

;-------------shapes---------------------

(define (cuboid x y z wid hig dep)
  (let ([a (iso (vector (- x wid) (+ y hig) (+ z dep)))]
        [b (iso (vector (- x wid) (+ y hig) (- z dep)))]
        [c (iso (vector (+ x wid) (+ y hig) (- z dep)))]
        [d (iso (vector (+ x wid) (+ y hig) (+ z dep)))]
        [e (iso (vector (- x wid) (- y hig) (+ z dep)))]
        [f (iso (vector (+ x wid) (- y hig) (+ z dep)))]
        [g (iso (vector (+ x wid) (- y hig) (- z dep)))]
        [h (iso (vector (- x wid) (- y hig) (- z dep)))])
    
    (define (iso-line p1 p2)
      (let-values ([(iso-x1 iso-y1) (ortho p1)]
                   [(iso-x2 iso-y2) (ortho p2)])
      (line iso-x1 iso-y1 iso-x2 iso-y2)))
      ;https://excalidraw.com/#json=Ah-neZtQgizt8Dq-ruEFR,aYpTJ47Sv3YkWp3E_ZWLVA
    
    (iso-line a b)
    (iso-line a d)
    (iso-line a e)
    (iso-line b c)
    (iso-line b h)
    (iso-line c d)
    (iso-line c g)
    (iso-line d f)
    (iso-line e h)
    (iso-line e f)
    (iso-line f g)
    (iso-line g h)))
    
;--------------drawing------------------

(define (setup)
  (fullscreen))

(define (draw)
  (background 0)
  (stroke 255)
  (translate (/ width  2)
             (/ height 2))
  
  (cuboid 0 0 0 100 100 30)))



 



    

    
    
