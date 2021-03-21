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
   relative-pos
   world-get-cell
   world-move-creature
   world-spawn-creature
   world-remove-creature
   world-find-creature
   world-add-wall
   world-add-rune
   make-blank-world
   make-world-from-file))

;; A position in the world.
(define-record-type <position>
  (make-pos x y) pos?
  (x pos-x)
  (y pos-y))

(define (pos-map-components p f)
  (f (pos-x p) (pos-y p)))

(define (relative-pos p dir)
  (pos-map-components
   p
   (case dir
     ((north) (lambda (x y) (make-pos x (+ y 1))))
     ((south) (lambda (x y) (make-pos x (- y 1))))
     ((east) (lambda (x y) (make-pos (+ x 1) y)))
     ((west) (lambda (x y) (make-pos (- x 1) y))))))

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

(define (make-blank-world size)
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
     ((eq? v #f) 'empty)
     (#t v))))

(define (world-cell-empty? world pos)
  (eq? (world-get-cell world pos) 'empty))

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
      (array-set! (world-cells world) 'wall (pos-x p) (pos-y p)))))

(define (world-add-rune world pos rune)
  (let ((p (world-wrap-position world pos)))
    (when (world-cell-empty? world p)
      (array-set! (world-cells world) (cons 'rune rune) (pos-x p) (pos-y p)))))

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
              (relative-pos pos move)
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

(test-case "moving creatures around the world"
  (let ((world (make-blank-world 10)))
    ;; there is nothing
    (assert-equal 'empty (world-get-cell world (make-pos 0 0)))
    ;; there is the player
    (world-spawn-creature world (make-pos 0 0) 'player 'bob)
    (assert-equal 'player (world-get-cell world (make-pos 0 0)))
    (assert-equal (make-pos 0 0) (world-find-creature world 'bob))
    ;; player moves north east
    (world-move-creature world (make-pos 0 0) (make-pos 1 1))
    (assert-equal
     (list 'empty 'player (make-pos 1 1))
     (list
      (world-get-cell world (make-pos 0 0))
      (world-get-cell world (make-pos 1 1))
      (world-find-creature world 'bob)))
    ;; player moves east
    (world-move-creature world (make-pos 1 1) 'east)
    (assert-equal
     (list 'empty 'player (make-pos 2 1))
     (list
      (world-get-cell world (make-pos 1 1))
      (world-get-cell world (make-pos 2 1))
      (world-find-creature world 'bob)))
    ;; player moves south
    (world-move-creature world 'bob 'south)
    (assert-equal
     (list 'empty 'player (make-pos 2 0))
     (list
      (world-get-cell world (make-pos 2 1))
      (world-get-cell world (make-pos 2 0))
      (world-find-creature world 'bob)))
    ;; Move off left edge.
    (world-move-creature world 'bob (make-pos 0 0))
    (world-move-creature world 'bob 'west)
    ;; Move him to a known position.
    (world-move-creature world 'bob (make-pos 2 1))
    ;; world wraps
    (assert-equal
     (list 'player 'player)
     (map (lambda (p) (world-get-cell world p))
          (list
           (make-pos 12 11)
           (make-pos -8 -9))))
    ;; player is gone
    (world-remove-creature world (make-pos 1 1))
    (assert-equal 'empty (world-get-cell world (make-pos 1 1)))
    ;; remove more than once doesn't break
    (world-remove-creature world (make-pos 1 1))
    (assert-equal 'empty (world-get-cell world (make-pos 1 1)))))



;; Returns an array suitable for use as world cells by
;; interpreting characters in STR.
(define (world-read-array str)
  (define (world-read-char char)
    (match char
      (#\space #f)
      (#\# 'wall)
      (_ #f)))
  (define (read-txt-to-array str)
    (let* ((lines (filter (negate string-null?) (string-split str #\linefeed)))
           (max-length (reduce max 1 (map string-length lines)))
           (pad (lambda (str) (string-pad-right str max-length #\space)))
           (padded (map pad lines)))
      (list->array 2 (reverse (map string->list padded)))))
  (let* ((src (read-txt-to-array str))
         (result (apply make-array #f (reverse (array-dimensions src)))))
    (array-index-map!
     result
     (lambda (i j)
       (world-read-char
        (array-ref src j i))))
    result))

(test-case "load world from file"
 (let ((w (make-world))
       (cells (world-read-array "###\n# #\n # ")))
   (world-set-cells! w cells)
   (assert-equal '(3 3) (array-dimensions cells))
   (let ((test (lambda (v x y)
                 (assert-equal v (world-get-cell w (make-pos x y))))))
     (test 'empty 0 0)
     (test 'wall 1 0)
     (test 'empty 2 0)
     (test 'wall 0 1)
     (test 'empty 1 1)
     (test 'wall 2 1)
     (test 'wall 0 2)
     (test 'wall 1 2)
     (test 'wall 2 2))))

(define (make-world-from-file file)
  (let* ((str (call-with-input-file file
                (lambda (port)
                  (get-string-all port))))
         (cells (world-read-array str))
         (w (make-world)))
    (world-set-cells! w cells)
    (world-set-creatures! w (make-hash-table 20))
    w))
