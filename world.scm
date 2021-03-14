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
   world-add-wall
   blank-world))

(use-modules
    (ice-9 match)
    (test))

;; A position in the world.
(define (make-pos x y)
  (cons x y))
(define pos-x car)
(define pos-y cdr)
(define (pos-map-components p f)
  (f (pos-x p) (pos-y p)))

;; Wrap a position in the world so it's valid.
(define (world-wrap-position world pos)
  (match-let (((w h) (array-dimensions world)))
    (let ((x (modulo (pos-x pos) w))
          (y (modulo (pos-y pos) h)))
      (make-pos x y))))

(define (world-add-wall world pos)
  (let ((p (world-wrap-position world pos)))
    (unless (world-cell-creature? world p)
      (array-set! world #t (pos-x p) (pos-y p)))))

;; Gets what's in the world at the provided position.
;; Returns #t for a wall, #f for nothing, or a
;; creature.
(define (world-get-cell world pos)
  (let ((p (world-wrap-position world pos)))
    (array-ref world (pos-x p) (pos-y p))))

(define (world-cell-empty? world pos)
  (let ((c (world-get-cell world pos)))
    (and (boolean? c) (not c))))

(define (world-cell-creature? world pos)
  (let ((c (world-get-cell world pos)))
    (not (boolean? c))))

;; Update world by moving the creature at pos to the
;; new position.
(define (world-move-creature world pos move)
  (let ((new-pos
         (if (symbol? move)
             (pos-map-components
              pos
              (case move
                ((north) (lambda (x y) (make-pos x (+ y 1))))
                ((south) (lambda (x y) (make-pos x (- y 1))))
                ((east) (lambda (x y) (make-pos (+ x 1) y)))
                ((west) (lambda (x y) (make-pos (- x 1) y)))))
             move)))
    (if (and (world-cell-creature? world pos)
             (world-cell-empty? world new-pos))
        (world-spawn-creature
         world new-pos (world-remove-creature world pos)))))

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

(test "player moves north east"
      (list #f 'player)
      (list
       (world-get-cell world (make-pos 0 0))
       (world-get-cell world (make-pos 1 1))))

(world-move-creature world (make-pos 1 1) 'east)

(test "player moves east"
      (list #f 'player)
      (list
       (world-get-cell world (make-pos 1 1))
       (world-get-cell world (make-pos 2 1))))

(test "world wraps"
      (list 'player 'player)
      (map (lambda (p) (world-get-cell world p))
           (list
            (make-pos 12 11)
            (make-pos -8 -9))))

(world-remove-creature world (make-pos 1 1))

(test "player is gone"
      #f
      (world-get-cell world (make-pos 1 1)))

(world-remove-creature world (make-pos 1 1))

(test "remove more than once doesn't break"
      #f
      (world-get-cell world (make-pos 1 1)))
