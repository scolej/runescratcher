(define-module (test game)
  #:use-module (util test)
  #:use-module (runes world)
  #:use-module (runes game)
  #:use-module (srfi srfi-26)
  #:export (run-all))

(define (make-test-game)
  (let ((g (make-game))
        (w (make-blank-world 5))
        (p0 (make-pos 0 0)))
    (world-spawn-creature w p0 'wizard 'player)
    (game-set-world! g w)
    (game-set-input-handler! g top-level)
    g))

(test-case write-runes
  "writing runes"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (p1 (make-pos 0 2))
         (r0 (make-pos 0 1)))
    (assert-equal p0 (world-find-creature w 'player))
    (assert-equal 'empty (world-cell-get w r0))
    ;; cancel 1
    (for-each input '(w escape))
    (assert-equal 'empty (world-cell-get w r0))
    ;; cancel 2
    (for-each input '(w a escape))
    (assert-equal 'empty (world-cell-get w r0))
    ;; write!
    (for-each input '(w a up))
    (assert-equal #t (rune? (world-get-entity-value w r0)))
    (assert-equal p1 (world-find-creature w 'player))))

(test-case rune-move-aoe
  "writing and move beyond the area of effect of a rune"
  (let* ((g (make-game))
         (w (make-blank-world 5))
         (input (cut game-input g <>))
         (pos make-pos)
         (add-wall (cut world-add-wall w <>))
         (move-assert
          (λ (i p)
            (input i)
            (assert-equal
             (list p 'player)
             (list (world-find-creature w 'player)
                   (world-get-entity-value w p))))))
    ;; setup
    ;; - there is a player, a blank space for a rune, and then a wall
    ;; - player will use the rune to move beyond the wall
    (game-set-input-handler! g top-level)
    (game-set-world! g w)
    (world-spawn-creature w (pos 2 0) 'wizard 'player)
    (for-each add-wall (list (pos 2 2) (pos 2 3)))
    ;; write!
    (for-each input '(w a up))
    ;; verify assumptions about rune effect
    ;; - player is on the other side of rune
    ;; - what was previously the player is now a wall
    (assert-equal (pos 2 2) (world-find-creature w 'player))
    (assert-equal 'wall (world-cell-get w (pos 2 0)))
    ;; try to move beyond rune effect area
    (move-assert 'up (pos 2 3))
    (move-assert 'up (pos 2 4))))

(define (run-all)
  ;; fixme (write-runes)
  ;; (rune-move-aoe)
  #t
  )
