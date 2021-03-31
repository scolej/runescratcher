(define-module (test world)
  #:use-module (runes pos)
  #:use-module (runes world)
  #:use-module (util test)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:export (move-things
            load-world
            all))

;; Assert that THING with NAME is at POS in WORLD.
(define (assert-pos world creature thing pos)
  (assert-equal creature (world-cell-get world pos))
  (assert-equal pos (world-find world thing)))

(test-case move-things
  "moving things around the world"
  (let* ((world (make-world-empty 10))
         (assert-pos (cut assert-pos world <> <> <>))
         (p0 (make-pos 0 0)))

    ;; there is nothing
    (assert-equal 'empty (world-cell-get world p0))

    ;; there is the player
    (world-spawn world p0 'player 'bob)
    (assert-pos 'player 'bob p0)

    ;; player moves north east
    (world-move world p0 (make-pos 1 1))
    (assert-equal 'empty (world-cell-get world (make-pos 0 0)))
    (assert-pos 'player 'bob (make-pos 1 1))

    ;; player moves east
    (world-move world (make-pos 1 1) 'east)
    (assert-equal 'empty (world-cell-get world (make-pos 1 1)))
    (assert-pos 'player 'bob (make-pos 2 1))

    ;; player moves south
    (world-move world 'bob 'south)
    (assert-equal 'empty (world-cell-get world (make-pos 2 1)))
    (assert-pos 'player 'bob (make-pos 2 0))

    ;; move off left edge, we don't crash
    (world-move world 'bob (make-pos 0 0))
    (world-move world 'bob 'west)

    ;; world does not wrap, out-of-bounds is walls
    (assert-equal 'wall (world-cell-get world (make-pos 12 11)))
    (assert-equal 'wall (world-cell-get world (make-pos -8 -9)))

    ;; player is gone
    (world-move world 'bob (make-pos 2 1))
    (world-remove world (make-pos 2 1))
    (assert-equal 'empty (world-cell-get world (make-pos 2 1)))
    (assert-equal #f (world-find world 'bob))

    ;; remove more than once doesn't break
    (world-remove world (make-pos 2 1))
    (assert-equal 'empty (world-cell-get world (make-pos 2 1)))))

(test-case load-world
  "load world from port"
  (let* ((w (call-with-input-string "###\n# #\n # " make-world-from-port))
         (test
          (λ (v x y)
            (assert-equal v (world-cell-get w (make-pos x y))))))
    (test 'empty 0 0)
    (test 'wall 1 0)
    (test 'empty 2 0)
    (test 'wall 0 1)
    (test 'empty 1 1)
    (test 'wall 2 1)
    (test 'wall 0 2)
    (test 'wall 1 2)
    (test 'wall 2 2)))

(define (all)
  (move-things)
  (load-world))
