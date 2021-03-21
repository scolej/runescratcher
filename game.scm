(define-module (game)
  #:use-module (world)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (test)
  #:export (game-input))

(define-record-type <game>
  (make-game) game?
  (input-handler game-input-handler game-set-input-handler!)
  (world game-world game-set-world!))

(define player-name 'player)



(define (arrow-to-cardinal-dir arr)
  (case arr
    ((left) 'west)
    ((right) 'east)
    ((up) 'north)
    ((down) 'south)))



(define (game-alert msg)
  (with-output-to-file "out.log"
    (lambda ()
      (format #t "---\n~a\n\n" msg))))

(define (game-input-back-to-top-level game)
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
    ((k)
     (game-set-input-handler! game (rune-direction-selection input)))
    (else
     (game-alert (format #f "no action for ~a" input)))))

(define (rune-direction-selection rune)
  (lambda (game input)
    (case input
      ((left right up down)
       (let* ((w (game-world game))
              (d (arrow-to-cardinal-dir input))
              (p (relative-pos (world-find-creature w player-name) d)))
         (world-add-rune w p rune)
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
         (p1 (make-pos 0 1)))
    (assert-equal p0 (world-find-creature w 'player))
    (assert-equal 'empty (world-get-cell w p1))
    ;; cancel 1
    (for-each input '(w escape))
    (assert-equal 'empty (world-get-cell w p1))
    ;; cancel 2
    (for-each input '(w k escape))
    (assert-equal 'empty (world-get-cell w p1))
    ;; write!
    (for-each input '(w k up))
    (assert-equal '(rune . k) (world-get-cell w p1))
    (assert-equal p0 (world-find-creature w 'player))))
