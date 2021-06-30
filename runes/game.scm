(define-module (runes game)
  #:use-module (util test)
  #:use-module (runes pos)
  #:use-module (runes runefs)
  #:use-module (runes trippy-world)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export
  (make-game
   make-game-with-world
   game-world
   game-input-handler
   game-set-world!
   game-set-input-handler!
   game-input
   top-level

   make-rune
   rune?
   game-add-rune))

(define-record-type <game>
  (make-game) game?
  (input-handler game-input-handler game-set-input-handler!)
  (world game-world game-set-world!))

(define (make-game-with-world world)
  (let ((g (make-game)))
    (game-set-world! g world)
    (game-set-input-handler! g top-level)
    g))

;; A symbol for identifying the player in the world.
(define player-name 'player)



(define (arrow->nsew arr)
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
           (d (arrow->nsew input)))
       (world-move w player-name d)))
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
              (d (arrow->nsew input))
              (p (relative-pos (world-find w player-name) d)))
         (add-rune w p (make-rune #\r 'flip))
         (game-input-back-to-top-level game)))
      (else
       (game-alert (format #f "no action for ~a" input))))))



(define-record-type <rune>
  (make-rune-raw id char effect) rune?
  ;; a symbol identifying this rune
  (id rune-id)
  ;; the character used to draw the rune in the world
  (char rune-char)
  ;; a symbol representing the effect of the rune
  (effect rune-effect))

;; Makes a rune with a new unique id.
(define make-rune
  (cut make-rune-raw (gensym "rune-") <> <>))

;; Adds RUNE into world W at position POS, additionally adding the
;; corresponding transform.
(define (add-rune w pos rune)
  (let ((effect (rune-effect rune))
        (id (rune-id rune))
        (x (pos-x pos))
        (y (pos-y pos)))
    (world-spawn w pos rune)
    (case effect
      ((flip)
       (let ((f (flipv y))
             (aoe (make-rectangle
                   (- x 2) (+ x 2) (- y 2) (+ y 2))))
         (world-add-transform w aoe f f id)))
      (else (error)))))
