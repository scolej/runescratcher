(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
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
   blank-world
   make-world-from-file))

;; A position in the world.
(define-record-type <position>
  (make-pos x y) pos?
  (x pos-x)
  (y pos-y))

(define (pos-map-components p f)
  (f (pos-x p) (pos-y p)))

(define (creature? c) (not (boolean? c)))

(define-record-type <world>
  (make-world)
  world?
  (cells world-cells world-set-cells!)
  ;; Hash from creature name to position in the world.
  (creatures world-creatures world-set-creatures!))

(define-record-type <named-creature>
  (make-named-creature name creature)
  named-creature?
  (name named-creature-name)
  (creature named-creature-creature))

(define (blank-world size)
  (let ((w (make-world)))
    (world-set-cells! w (make-array #f size size))
    (world-set-creatures! w (make-hash-table 20))
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
            (hash-set! (world-creatures world) name p)
            #t))))
   ;; Anonymous creature.
   ((world pos creature)
    (world-spawn-creature
     world pos creature (gensym "anon-creature-")))))

(define (world-find-creature world name)
  (hash-ref (world-creatures world) name))

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
(define (world-move-creature world what move)
  (let* ((pos
          (cond
           ((symbol? what) (world-find-creature world what))
           ((pos? what) what)
           (#t (error ":["))))
         (new-pos
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
        (let* ((pw (world-wrap-position world pos))
               (px (pos-x pw))
               (py (pos-y pw))
               (npw (world-wrap-position world new-pos))
               (npx (pos-x npw))
               (npy (pos-y npw))
               (nc (array-ref (world-cells world) px py)))
          (array-set! (world-cells world) #f px py)
          (array-set! (world-cells world) nc npx npy)
          (hash-set! (world-creatures world) (named-creature-name nc) new-pos)))))

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
       (world-find-creature world 'bob)))

(world-move-creature world (make-pos 0 0) (make-pos 1 1))

(test "player moves north east"
      (list #f 'player (make-pos 1 1))
      (list
       (world-get-cell world (make-pos 0 0))
       (world-get-cell world (make-pos 1 1))
       (world-find-creature world 'bob)))

(world-move-creature world (make-pos 1 1) 'east)

(test "player moves east"
      (list #f 'player (make-pos 2 1))
      (list
       (world-get-cell world (make-pos 1 1))
       (world-get-cell world (make-pos 2 1))
       (world-find-creature world 'bob)))

(world-move-creature world 'bob 'south)

(test "player moves south"
      (list #f 'player (make-pos 2 0))
      (list
       (world-get-cell world (make-pos 2 1))
       (world-get-cell world (make-pos 2 0))
       (world-find-creature world 'bob)))

;; Move off left edge.
(world-move-creature world 'bob (make-pos 0 0))
(world-move-creature world 'bob 'west)

;; Move him to a known position.
(world-move-creature world 'bob (make-pos 2 1))

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

;;
;;
;;

(define (read-txt-to-array str)
  (let* ((lines (filter (negate string-null?) (string-split str #\linefeed)))
         (max-length (reduce max 1 (map string-length lines)))
         (pad (lambda (str) (string-pad-right str max-length #\space)))
         (padded (map pad lines)))
    (list->array 2 (reverse (map string->list padded)))))

(test "simple read"
      array-equal?
      (list->array 2 '((#\e #\f #\space)
                       (#\d #\space #\space)
                       (#\a #\b #\c)))
      (read-txt-to-array "abc\nd\nef"))

(define (world-read-char char)
  (match char
    (#\space #f)
    (#\# #t)
    (_ #f)))

;; Returns an array suitable for use as world cells by
;; interpreting characters in the array SRC.
(define (world-read-array src)
  (let ((result (apply make-array #f (reverse (array-dimensions src)))))
    (array-index-map!
     result
     (lambda (i j)
       (world-read-char
        (array-ref src j i))))
    result))

(let ((w (make-world))
      (cells (world-read-array
              (read-txt-to-array "###\n# #\n # "))))
  (world-set-cells! w cells)
  (test "" '(3 3) (array-dimensions cells))
  (test "" #f (world-get-cell w (make-pos 0 0)))
  (test "" #t (world-get-cell w (make-pos 1 0)))
  (test "" #f (world-get-cell w (make-pos 2 0)))

  (test "" #t (world-get-cell w (make-pos 0 1)))
  (test "" #f (world-get-cell w (make-pos 1 1)))
  (test "" #t (world-get-cell w (make-pos 2 1)))

  (test "" #t (world-get-cell w (make-pos 0 2)))
  (test "" #t (world-get-cell w (make-pos 1 2)))
  (test "" #t (world-get-cell w (make-pos 2 2))))

(define (make-world-from-file file)
  (let* ((str (call-with-input-file file
                (lambda (port)
                  (get-string-all port))))
         (cells (world-read-array
                 (read-txt-to-array
                  str)))
         (w (make-world)))
    (world-set-cells! w cells)
    (world-set-creatures! w (make-hash-table 20))
    w))
