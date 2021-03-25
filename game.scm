(define-module (game)
  #:use-module (world)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (test)
  #:export (make-game-with-world
            game-input))

(define-record-type <game>
  (make-game) game?
  (input-handler game-input-handler game-set-input-handler!)
  (world game-world game-set-world!))

(define (make-game-with-world world)
  (let ((g (make-game)))
    (game-set-world! g world)
    (game-set-input-handler! g top-level)
    g))

(define player-name 'player)



(define (arrow-to-cardinal-dir arr)
  (case arr
    ((left) 'west)
    ((right) 'east)
    ((up) 'north)
    ((down) 'south)))



(define (game-alert msg)
  (let ((port (open-file "out.log" "a")))
    (format port "---\n~a\n\n" msg)
    (force-output port)
    (close-port port)))

(define (game-input-back-to-top-level game)
  (game-alert "back to top level")
  (game-set-input-handler! game top-level))

;;;
;;; Input matchers
;;;
;;; Each procedure accepts the game and the new input
;;; and can update the game appropriately.
;;;

(define (game-input game input)
  ((game-input-handler game) game input))

(define (top-level game input)
  (case input
    ((left right up down)
     (let ((w (game-world game))
           (d (arrow-to-cardinal-dir input)))
       (world-move-creature w player-name d)))
    ((w)
     (game-set-input-handler! game rune-selection))
    (else
     (game-alert (format #f "no action for ~a" input)))))

(define (rune-selection game input)
  (case input
    ((escape)
     (game-input-back-to-top-level game))
    ((a s d f)
     (game-set-input-handler! game (rune-direction-selection input)))
    (else
     (game-alert (format #f "no action for ~a" input)))))

(define (rune-direction-selection rune-char)
  (lambda (game input)
    (case input
      ((left right up down)
       (let* ((w (game-world game))
              (d (arrow-to-cardinal-dir input))
              (p (relative-pos (world-find-creature w player-name) d)))
         (world-add-rune w p (make-rune p))
         (game-input-back-to-top-level game)))
      (else
       (game-alert (format #f "no action for ~a" input))))))



(define (make-test-game)
  (let ((g (make-game))
        (w (make-blank-world 5))
        (p0 (make-pos 0 0)))
    (world-spawn-creature w p0 'wizard 'player)
    (game-set-world! g w)
    (game-set-input-handler! g top-level)
    g))

(test-case "writing runes"
  (let* ((g (make-test-game))
         (w (game-world g))
         (input (cut game-input g <>))
         (p0 (make-pos 0 0))
         (p1 (make-pos 0 2))
         (r0 (make-pos 0 1)))
    (assert-equal p0 (world-find-creature w 'player))
    (assert-equal 'empty (world-get-cell w r0))
    ;; cancel 1
    (for-each input '(w escape))
    (assert-equal 'empty (world-get-cell w r0))
    ;; cancel 2
    (for-each input '(w a escape))
    (assert-equal 'empty (world-get-cell w r0))
    ;; write!
    (for-each input '(w a up))
    (assert-equal #t (rune? (world-get-entity-value w r0)))
    (assert-equal p1 (world-find-creature w 'player))))

(test-case "writing and move beyond the area of effect of a rune"
  (let* ((g (make-game))
         (w (make-blank-world 5))
         (input (cut game-input g <>))
         (pos make-pos)
         (add-wall (cut world-add-wall w <>))
         (move-assert
          (λ (i p)
            (input i)
            (assert-equal 'player (world-get-entity-value w p))
            (assert-equal p (world-find-creature w 'player)))))
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
    (assert-equal 'wall (world-get-cell w (pos 2 0)))
    ;; try to move beyond rune effect area
    (move-assert 'up (pos 2 3))
    (move-assert 'up (pos 2 4))))
