(define-module (test game)
  #:use-module (srfi srfi-26)
  #:use-module (util test)
  #:use-module (runes pos)
  #:use-module (runes trippy-world)
  #:use-module (runes game)
  #:export
  (all))

;; A test game: an empty world with a wizard player at the south-west corner.
(define (make-test-game)
  (let ((g (make-game))
        (w (make-world-empty 5))
        (p0 (make-pos 0 0)))
    (world-spawn w p0 'wizard 'player)
    (game-set-world! g w)
    (game-set-input-handler! g top-level)
    g))

(define (move-assert game input new-position)
  (game-input game input)
  (let ((w (game-world game)))
    (assert-equal 'wizard (world-cell-get w new-position))
    (assert-equal new-position (world-find w 'player))))

(test-case simple-movement
  "player can move around, but not into walls"
  (let* ((g (make-test-game))
         (move-assert (cut move-assert g <> <>)))
    (move-assert 'up (make-pos 0 1))
    (move-assert 'right (make-pos 1 1))
    (move-assert 'down (make-pos 1 0))
    (move-assert 'left (make-pos 0 0))
    (move-assert 'left (make-pos 0 0))))

(test-case write-runes
  "writing runes"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (p1 (make-pos 0 2))
         (r0 (make-pos 0 1)))
    (assert-equal p0 (world-find w 'player))
    (assert-equal 'empty (world-cell-get w r0))
    ;; cancel 1
    (for-each input '(w escape))
    (assert-equal 'empty (world-cell-get w r0))
    ;; cancel 2
    (for-each input '(w a escape))
    (assert-equal 'empty (world-cell-get w r0))
    ;; write!
    (for-each input '(w a up))
    (assert-equal #t (rune? (world-cell-get w r0)))
    (assert-equal p1 (world-find w 'player))))

(test-case rune-move-aoe
  "writing and move beyond the area of effect of a rune"
  (let* ((g (make-game))
         (w (make-world-empty 5))
         (input (cut game-input g <>))
         (pos make-pos)
         (add-wall (cut world-add-wall w <>))
         (move-assert
          (λ (i p)
            (input i)
            (assert-equal
             (list p 'player)
             (list (world-find w 'player)
                   (world-cell-get w p))))))
    ;; setup
    ;; - there is a player, a blank space for a rune, and then a wall
    ;; - player will use the rune to move beyond the wall
    (game-set-input-handler! g top-level)
    (game-set-world! g w)
    (world-spawn w (pos 2 0) 'wizard 'player)
    (for-each add-wall (list (pos 2 2) (pos 2 3)))
    ;; write!
    (for-each input '(w a up))
    ;; verify assumptions about rune effect
    ;; - player is on the other side of rune
    ;; - what was previously the player is now a wall
    (assert-equal (pos 2 2) (world-find w 'player))
    (assert-equal 'wall (world-cell-get w (pos 2 0)))
    ;; try to move beyond rune effect area
    (move-assert 'up (pos 2 3))
    (move-assert 'up (pos 2 4))))

(define (all)
  (simple-movement)
  ;; fixme (write-runes)
  ;; (rune-move-aoe)
  )
