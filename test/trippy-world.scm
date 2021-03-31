(define-module (test trippy-world)
  #:use-module (util test)
  #:use-module (runes pos)
  #:use-module (runes trippy-world)
  #:export
  (all))

(define (make-test-world)
  (let ((w (make-world-empty 5)))
    w))

(test-case flip-vert
  "a transformation to mirror vertically"
  (let ((w (make-test-world))
        (walls-before
         (list (make-pos 0 0)
               (make-pos 1 1)
               (make-pos 2 2)
               (make-pos 3 3)
               (make-pos 4 4)))
        (walls-after
         (list (make-pos 0 0)
               (make-pos 1 3)
               (make-pos 2 2)
               (make-pos 3 1)
               (make-pos 4 4))))
    (for-each (λ (p) (world-add-wall w p)) walls-before)
    (world-add-transform
     w (make-rectangle 1 3 1 3)
     (λ (x y) (make-pos x (- (* 2 2) y))))
    (for-each (λ (p)
                (assert-equal 'wall (world-cell-get w p)))
              walls-after)))

(define (all)
  (flip-vert))
