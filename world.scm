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
  ;; Hash from unit name to position in the world.
  (units world-units world-set-units!))

(define-record-type <named-creature>
  (make-named-creature name creature)
  named-creature?
  (name named-creature-name)
  (creature named-creature-creature))

(define (blank-world size)
  (let ((w (make-world)))
    (world-set-cells! w (make-array #f size size))
    (world-set-units! w (make-hash-table 20))
    w))

;; Gets what's in the world at the provided position.
;; Returns #t for a wall, #f for nothing, or a
;; creature.
(define (world-get-cell world pos)
  (let* ((p (world-wrap-position world pos))
         (v (array-ref (world-cells world) (pos-x p) (pos-y p))))
    (cond
     ((named-creature? v) (named-creature-creature v))
     (#t v))))

(define (world-cell-empty? world pos)
  (let ((c (world-get-cell world pos)))
    (and (boolean? c) (not c))))

(define (world-cell-creature? world pos)
  (let* ((p (world-wrap-position world pos))
         (px (pos-x p))
         (py (pos-y p)))
    (named-creature?
     (array-ref (world-cells world) px py))))

;; Introduce a new creature into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define world-spawn-creature
  (case-lambda
   ;; Allow naming the creature for later look-up.
   ((world pos creature name)
    (let* ((p (world-wrap-position world pos))
           (x (pos-x p))
           (y (pos-y p)))
      (if (not (world-cell-empty? world p)) #f
          (begin
            (array-set!
             (world-cells world)
             (make-named-creature name creature)
             x y)
            (hash-set! (world-units world) name p)
            #t))))
   ;; Anonymous creature.
   ((world pos creature)
    (world-spawn-creature
     world pos creature (gensym "anon-creature-")))))

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
        (let* ((px (pos-x pos))
               (py (pos-y pos))
               (npx (pos-x new-pos))
               (npy (pos-y new-pos))
               (nc (array-ref (world-cells world) px py)))
          (array-set! (world-cells world) #f px py)
          (array-set! (world-cells world) nc npx npy)
          (hash-set! (world-units world) (named-creature-name nc) new-pos)))))

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

(world-spawn-creature world (make-pos 0 0) 'player 'bob)

(test "there is the player"
      (list 'player (make-pos 0 0))
      (list
       (world-get-cell world (make-pos 0 0))
       (world-find-unit world 'bob)))

(world-move-creature world (make-pos 0 0) (make-pos 1 1))

(test "player moves north east"
      (list #f 'player (make-pos 1 1))
      (list
       (world-get-cell world (make-pos 0 0))
       (world-get-cell world (make-pos 1 1))
       (world-find-unit world 'bob)))

(world-move-creature world (make-pos 1 1) 'east)

(test "player moves east"
      (list #f 'player (make-pos 2 1))
      (list
       (world-get-cell world (make-pos 1 1))
       (world-get-cell world (make-pos 2 1))
       (world-find-unit world 'bob)))

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
