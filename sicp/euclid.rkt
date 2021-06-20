#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
  )
)


;Applicative
;(gcd 204 40)
;(gcd 40 (remainder 204 40))
;(gcd 40 4)
;(gcd 4 (remainder 40 4 ))
;(gcd 4 0)
;4


