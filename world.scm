(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
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

;; A position in the world.
(define (make-pos x y)
  (cons x y))
(define pos-x car)
(define pos-y cdr)
(define (pos-map-components p f)
  (f (pos-x p) (pos-y p)))

(define (creature? c) (not (boolean? c)))

(define-record-type <world>
  (make-world) world?
  (cells world-cells world-set-cells!)
  (units world-units world-set-units!))

(define (blank-world size)
  (let ((w (make-world)))
    (world-set-cells! w (make-array #f size size))
    (world-set-units! w (make-hash-table 20))
    w))

(define (world-name-creature world pos name)
  (let ((c (world-get-cell world pos)))
    (if (creature? c)
        (hash-set! (world-units world)
                   name pos))))

(define (world-find-unit world name)
  (hash-ref (world-units world) name))

;; Wrap a position in the world so it's valid.
(define (world-wrap-position world pos)
  (match-let (((w h) (array-dimensions (world-cells world))))
    (let ((x (modulo (pos-x pos) w))
          (y (modulo (pos-y pos) h)))
      (make-pos x y))))

(define (world-add-wall world pos)
  (let ((p (world-wrap-position world pos)))
    (unless (world-cell-creature? world p)
      (array-set! (world-cells world) #t (pos-x p) (pos-y p)))))

;; Gets what's in the world at the provided position.
;; Returns #t for a wall, #f for nothing, or a
;; creature.
(define (world-get-cell world pos)
  (let ((p (world-wrap-position world pos)))
    (array-ref (world-cells world) (pos-x p) (pos-y p))))

(define (world-cell-empty? world pos)
  (let ((c (world-get-cell world pos)))
    (and (boolean? c) (not c))))

(define (world-cell-creature? world pos)
  (creature? (world-get-cell world pos)))

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
        (let ((c (world-remove-creature world pos)))
          (world-spawn-creature world new-pos c)
          ;; TODO cooked in assumption that a unit is its name
          ;; It's name can be different.
          (when (hash-ref (world-units world) c)
            (hash-set! (world-units world) c new-pos))))))

;; Introduce a new creature into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define (world-spawn-creature world pos creature)
  (let* ((p (world-wrap-position world pos))
         (x (pos-x p))
         (y (pos-y p)))
    (if (array-ref (world-cells world) x y) #f
        (begin
          (array-set! (world-cells world) creature x y)
          #t))))

(define (world-remove-creature world pos)
  (let* ((p (world-wrap-position world pos))
         (x (pos-x p))
         (y (pos-y p))
         (c (array-ref (world-cells world) x y)))
    (if (boolean? c) #f
        (begin
          (array-set! (world-cells world) #f x y)
          c))))

;;
;;

(define world (blank-world 10))

(test "there is nothing"
      #f
      (world-get-cell world (make-pos 0 0)))

(world-spawn-creature world (make-pos 0 0) 'player)
(world-name-creature world (make-pos 0 0) 'player)

(test "there is the player"
      (list 'player (make-pos 0 0))
      (list
       (world-get-cell world (make-pos 0 0))
       (world-find-unit world 'player)))

(world-move-creature world (make-pos 0 0) (make-pos 1 1))

(test "player moves north east"
      (list #f 'player (make-pos 1 1))
      (list
       (world-get-cell world (make-pos 0 0))
       (world-get-cell world (make-pos 1 1))
       (world-find-unit world 'player)))

(world-move-creature world (make-pos 1 1) 'east)

(test "player moves east"
      (list #f 'player (make-pos 2 1))
      (list
       (world-get-cell world (make-pos 1 1))
       (world-get-cell world (make-pos 2 1))
       (world-find-unit world 'player)))

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
