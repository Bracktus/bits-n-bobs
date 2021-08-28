#lang racket

(define (insert val lst)
  (let loop ([val val]
             [left '()]
             [right lst]
             [acc '()])
    (if (pair? right)

        (loop val 
              (append left (list (car right)))
              (cdr right)
              (append acc (list (append left (cons val right)))))

        (append acc (list (append left (cons val right)))))))

(define (permutations lst)
    (if (null? lst)
      '(())
      ;for each permutation in permutations-of-rest (which is a list)
      ;insert head into each position between permutation's elements and also on both its ends,
      ;unless permutation was empty. In which case the string with one element, head, is the sole resulting permutation.
      (apply append
             (map (λ (perm-list) (insert (car lst) perm-list))
                  (permutations (cdr lst))))))

(permutations '(3 4 5))
