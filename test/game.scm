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
    (for-each input '(a escape))
    (assert-equal 'empty (world-cell-get w r0))
    ;; write!
    (for-each input '(a up))
    (assert-equal #t (rune? (world-cell-get w r0)))
    (assert-equal p1 (world-find w 'player))))

(test-case no-runes-on-walls
  "cannot write rules on walls"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (wp (make-pos 0 1)))
    (world-add-wall w wp)
    (assert-equal p0 (world-find w 'player))
    ;; try to write rune on wall
    (for-each input '(a up))
    (assert-equal 'wall (world-cell-get w wp))
    (assert-equal p0 (world-find w 'player))))

(test-case write-and-remove-1
  "writing & removing runes where everything stays the same"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (p1 (make-pos 2 0))
         (r0 (make-pos 1 0)))
    (assert-equal p0 (world-find w 'player))
    (assert-equal 'empty (world-cell-get w r0))
    ;; write a rune north of player
    (for-each input '(a right))
    (assert-equal #t (rune? (world-cell-get w r0)))
    (assert-equal p0 (world-find w 'player))
    ;; erase rune (which is east of player)
    (for-each input '(e right))
    ;; rune is gone and player is in same position
    (assert-equal 'empty (world-cell-get w r0))
    (assert-equal p0 (world-find w 'player))))

(test-case write-and-remove-2
  "writing & removing runes where removal alters player's position"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (p1 (make-pos 0 2))
         (r0 (make-pos 0 1)))
    (assert-equal p0 (world-find w 'player))
    (assert-equal 'empty (world-cell-get w r0))
    ;; write a rune north of player
    (for-each input '(a up))
    (assert-equal #t (rune? (world-cell-get w r0)))
    (assert-equal p1 (world-find w 'player))
    ;; erase rune (which is now south of player)
    (for-each input '(e down))
    ;; rune is gone and player is back where they started
    (assert-equal 'empty (world-cell-get w r0))
    (assert-equal p0 (world-find w 'player))))

(test-case rune-move-aoe
  "writing and move beyond the area of effect of a rune"
  (let* ((g (make-game))
         (w (make-world-empty 7))
         (input (cut game-input g <>))
         (pos make-pos)
         (add-wall (cut world-add-wall w <>))
         (move-assert
          ;; give input I to the game, and assert player position is P
          (λ (i p)
            (input i)
            (assert-equal
             (list p 'wizard)
             (list (world-find w 'player)
                   (world-cell-get w p))))))
    ;; setup
    ;; - there is a player, a blank space for a rune, and then a wall
    ;; - player will use the rune to move beyond the wall
    (game-set-input-handler! g top-level)
    (game-set-world! g w)
    (world-spawn w (pos 3 0) 'wizard 'player)
    (for-each add-wall (list (pos 3 4) (pos 3 5)))
    ;; move up so we can write the rune at the centre of the world: 3, 3
    (for-each input '(up up))
    (assert-equal (pos 3 2) (world-find w 'player))
    ;; write the rune!
    (for-each input '(w a up))
    ;; verify assumptions about rune effect
    ;; - player is on the other side of rune
    ;; - what was previously the player is now a wall
    (assert-equal (pos 3 4) (world-find w 'player))
    (assert-equal 'wall (world-cell-get w (pos 3 2)))
    ;; try to move beyond rune effect area
    (move-assert 'up (pos 3 5))
    (move-assert 'up (pos 3 6))))

(define (all)
  (simple-movement)
  (write-runes)
  (no-runes-on-walls)
  (write-and-remove-1)
  (write-and-remove-2)
  (rune-move-aoe))
