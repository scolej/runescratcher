(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (test)
  #:export
  (make-pos
   pos-x
   pos-y
   world-get-cell
   world-move-creature
   world-spawn-creature
   world-remove-creature
   world-set-cell
   blank-world))

(use-modules
    (ice-9 match)
    (test))

;; A position in the world.
(define (make-pos x y)
  (cons x y))
(define pos-x car)
(define pos-y cdr)

;; Wrap a position in the world so it's valid.
(define (world-wrap-position world pos)
  (match-let (((w h) (array-dimensions world)))
    (let ((x (modulo (pos-x pos) w))
          (y (modulo (pos-y pos) h)))
      (make-pos x y))))

(define (world-set-cell world pos v)
  (let ((p (world-wrap-position world pos)))
    (array-set! world v (pos-x p) (pos-y p))))

;; Gets what's in the world at the provided position.
;; Returns #t for a wall, #f for nothing, or a
;; creature.
(define (world-get-cell world pos)
  (let ((p (world-wrap-position world pos)))
    (array-ref world (pos-x p) (pos-y p))))

;; Update world by moving the creature at pos to the
;; new position.
(define (world-move-creature world pos new-pos)
  (let ((c (world-remove-creature world pos)))
    (unless (boolean? c)
      (world-spawn-creature world new-pos c))))

;; Introduce a new creature into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define (world-spawn-creature world pos creature)
  (let* ((p (world-wrap-position world pos))
         (x (pos-x p))
         (y (pos-y p)))
    (if (array-ref world x y) #f
        (begin
          (array-set! world creature x y)
          #t))))

(define (world-remove-creature world pos)
  (let* ((p (world-wrap-position world pos))
         (x (pos-x p))
         (y (pos-y p))
         (c (array-ref world x y)))
    (if (boolean? c) #f
        (begin
          (array-set! world #f x y)
          c))))

(define (blank-world size)
  (make-array #f size size))

;;
;;

(define world (blank-world 10))

(test "there is nothing"
      #f
      (world-get-cell world (make-pos 0 0)))

(world-spawn-creature world (make-pos 0 0) 'player)

(test "there is the player"
      'player
      (world-get-cell world (make-pos 0 0)))

(world-move-creature world (make-pos 0 0) (make-pos 1 1))

(test "player is not at old position"
      #f
      (world-get-cell world (make-pos 0 0)))

(test "player is at new position"
      'player
      (world-get-cell world (make-pos 1 1)))

(test "world wraps"
      'player
      (world-get-cell world (make-pos 11 11)))

(world-remove-creature world (make-pos 1 1))

(test "player is gone"
      #f
      (world-get-cell world (make-pos 1 1)))

(world-remove-creature world (make-pos 1 1))

(test "can't remove more than once"
      #f
      (world-get-cell world (make-pos 1 1)))
