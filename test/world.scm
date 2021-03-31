(define-module (test world)
  #:use-module (runes pos)
  #:use-module (runes world)
  #:use-module (util test)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:export (run-all))

;; Assert that CREATURE with NAME is at POS in WORLD.
(define (assert-creature-position world creature name pos)
  (assert-equal creature (world-get-entity-value world pos))
  (assert-equal pos (world-find-creature world name)))

;; Assert that RUNE with NAME is at POS in WORLD.
(define (assert-rune-position world rune name pos)
  (assert-equal rune (world-get-entity-value world pos))
  (assert-equal pos (world-find-rune world name)))

(test-case move-creatures
  "moving creatures around the world"
  (let ((world (make-blank-world 10)))
    ;; there is nothing
    (assert-equal 'empty (world-cell-get world (make-pos 0 0)))
    ;; there is the player
    (world-spawn-creature world (make-pos 0 0) 'player 'bob)
    (assert-equal 'player (world-get-entity-value world (make-pos 0 0)))
    (assert-equal (make-pos 0 0) (world-find-creature world 'bob))
    ;; player moves north east
    (world-move-creature world (make-pos 0 0) (make-pos 1 1))
    (assert-equal 'empty (world-cell-get world (make-pos 0 0)))
    (assert-equal 'player (world-get-entity-value world (make-pos 1 1)))
    (assert-equal (make-pos 1 1) (world-find-creature world 'bob))
    ;; player moves east
    (world-move-creature world (make-pos 1 1) 'east)
    (assert-equal 'empty (world-cell-get world (make-pos 1 1)))
    (assert-equal 'player (world-get-entity-value world (make-pos 2 1)))
    (assert-equal (make-pos 2 1) (world-find-creature world 'bob))
    ;; player moves south
    (world-move-creature world 'bob 'south)
    (assert-equal 'empty (world-cell-get world (make-pos 2 1)))
    (assert-equal 'player (world-get-entity-value world (make-pos 2 0)))
    (assert-equal (make-pos 2 0) (world-find-creature world 'bob))
    ;; Move off left edge.
    (world-move-creature world 'bob (make-pos 0 0))
    (world-move-creature world 'bob 'west)
    ;; Move him to a known position.
    (world-move-creature world 'bob (make-pos 2 1))
    ;; world does not wrap, out-of-bounds is walls
    (assert-equal #t (world-cell-get world (make-pos 12 11)))
    (assert-equal #t (world-cell-get world (make-pos -8 -9)))
    ;; player is gone
    (world-remove-creature world (make-pos 1 1))
    (assert-equal 'empty (world-cell-get world (make-pos 1 1)))
    ;; remove more than once doesn't break
    (world-remove-creature world (make-pos 1 1))
    (assert-equal 'empty (world-cell-get world (make-pos 1 1)))))

(test-case add-remove-rune
  "add and remove a rune"
  (let* ((w (make-blank-world 3))
         (p (make-pos 1 1))             ; position for a rune
         (r (make-rune p))              ; rune value
         (n 'that-rune))                ; rune name
    (assert-equal #f (world-find-rune w n))
    ;; add
    (world-add-rune w p r n)
    (assert-rune-position w r n p)
    ;; remove
    (world-remove-rune w n)
    (assert-equal #f (world-find-rune w n))
    (assert-equal 'empty (world-cell-get w p))))

(test-case flip-rune
  "flip rune"
  (let* ((size 5)
         (w (make-blank-world size))
         (p0 (make-pos 2 1))            ; player pos before
         (r0 (make-pos 2 2))            ; rune pos
         (p1 (make-pos 2 3))            ; player pos after
         ;; a function to flip a position vertically about rune
         (f (cute pos-map-components <>
                  (λ (x y) (make-pos x (- (* 2 (pos-y r0)) y)))))
         (walls (list (make-pos 1 1)
                      (make-pos 1 2)
                      (make-pos 1 3)
                      (make-pos 3 3)
                      (make-pos 3 2)
                      (make-pos 3 1)))
         (walls-after (map f walls)))
    ;; setup
    (world-spawn-creature w p0 'wizard 'player)
    (world-spawn-creature w p1 'lizard 'anon-lizard)
    (for-each
     (λ (p)
       (world-add-wall w p))
     walls)
    ;; add rune
    (world-add-rune w r0 (make-rune r0))
    ;; walls and creatures are flipped
    (for-each
     (λ (pos)
       (assert-equal 'wall (world-cell-get w pos)))
     walls-after)
    (assert-creature-position w 'wizard 'player p1)
    (assert-creature-position w 'lizard 'anon-lizard p0)))

;; todo
;; - verify what's outside aoe
;; - verify add/remove rune/creature in presence of rune

(test-case load-world
  "load world from file"
  (call-with-bt-exn
   (λ ()
     (let ((w (make-world))
           (cells (world-read-array "###\n# #\n # ")))
       (world-set-cells! w cells)
       (world-set-runes! w (make-hash-table))
       (assert-equal '(3 3) (array-dimensions cells))
       (let ((test (lambda (v x y)
                     (assert-equal v (world-cell-get w (make-pos x y))))))
         (test 'empty 0 0)
         (test 'wall 1 0)
         (test 'empty 2 0)
         (test 'wall 0 1)
         (test 'empty 1 1)
         (test 'wall 2 1)
         (test 'wall 0 2)
         (test 'wall 1 2)
         (test 'wall 2 2))))))

(define (run-all)
  (move-creatures)
  (add-remove-rune)
  ;; (flip-rune)
  (load-world))
