(define-module (util random)
  #:export
  (comparing))

(define (comparing f g)
  (λ (a b)
    (f (g a) (g b))))
