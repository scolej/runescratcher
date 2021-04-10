(define-module (test trippy-world)
  #:use-module (srfi srfi-26)
  #:use-module (util test)
  #:use-module (runes pos)
  #:use-module (runes trippy-world)
  #:export
  (all))

(define (make-test-world)
  (let ((w (make-world-empty 5)))
    w))

(define (add-flip-transform w)
  (let ((f (cut pos-map-components <>
                (λ (x y)
                  (make-pos x (- (* 2 2) y))))))
    (world-add-transform
     w (make-rectangle 1 3 1 3) f f)))

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
    (add-flip-transform w)
    (for-each
      (λ (p)
         (assert-equal 'wall (world-cell-get w p)))
      walls-after)))

(define (assert-pos world position)
  ;; fixme backtrace site still missing!
  ;; is it the lambda?
  ;; simple "expectation" to get richer fails
  ;; (display position) (newline)
  (assert-equal position (world-find world 'player))
  (assert-equal 'wizard (world-cell-get world position)))

(test-case flip-vert-move-through
  "unit moves into, through, and out of, a vertical mirror"
  (let* ((w (make-test-world))
         (move-north
          (λ (new-pos)
            (world-move w 'player 'north)
            (assert-pos w new-pos)
            #f)))
    (world-spawn w (make-pos 2 0) 'wizard 'player)
    (add-flip-transform w)
    (move-north (make-pos 2 1))
    (move-north (make-pos 2 2))
    (move-north (make-pos 2 3))
    (move-north (make-pos 2 4))))

(test-case remove-transform
  "move a unit into a transform, then remove the transform"
  (let* ((w (make-test-world))
         (move-north
          (λ (new-pos)
            (world-move w 'player 'north)
            (assert-pos w new-pos)
            #f))
         (t #f))
    (world-spawn w (make-pos 2 0) 'wizard 'player)
    (set! t (add-flip-transform w))
    (move-north (make-pos 2 1))
    (world-remove-transform w t)
    (assert-pos w (make-pos 2 3))))

(test-case add-transform-over-unit
  "add a transform on top of a unit"
  (let* ((w (make-test-world))
         (move-north
          (λ (new-pos)
            (world-move w 'player 'north)
            (assert-pos w new-pos)
            #f))
         (t #f))
    (world-spawn w (make-pos 2 1) 'wizard 'player)
    (add-flip-transform w)
    (assert-pos w (make-pos 2 3))))

;; fixme next cases
;;
;; - add a named thing inside rect of transform
;; - move a thing from inside to outside with abs pos
;; - move a thing from inside to outside with direction
;; - ... and outside to inside
;;
;; better test errors, how?

(define (all)
  (flip-vert)
  (flip-vert-move-through)
  (remove-transform)
  (add-transform-over-unit))
