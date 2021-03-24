(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-42)
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



(define (msg level str . args)
  (apply format #t str args))



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

;; An entity is an object in the world decorated with a name. A name is just a
;; unique symbol which provides a way to refer to the entity.
(define-record-type <entity>
  (make-entity name value)
  entity?
  (name entity-name)
  (value entity-value))



(define-record-type <world>
  (make-world) world?
  (cells world-cells world-set-cells!)
  ;; Hash from creature name to position in the world.
  (creatures world-creatures world-set-creatures!)
  ;; List of runes
  (runes world-runes world-set-runes!))

(define (make-blank-world size)
  (let ((w (make-world)))
    (world-set-cells! w (make-array #f size size))
    (world-set-creatures! w (make-hash-table 20))
    (world-set-runes! w (make-hash-table 20))
    w))

;; Set the value of the cell at POS to VAL.
;; No checking, no position transform, no nothing.
;; Use with care.
(define (world-cell-set! world val pos)
  (array-set! (world-cells world) val (pos-x pos) (pos-y pos)))

;; Gets what's in the world at the provided position.
;; Returns #t for a wall, #f for nothing, or a
;; creature.
(define (world-get-cell world pos)
  (let* ((p (world-wrap-position world pos))
         (v (array-ref (world-cells world) (pos-x p) (pos-y p))))
    (cond
     ((entity? v) (entity-value v))
     ((eq? v #f) 'empty)
     (#t v))))

(define (world-cell-empty? world pos)
  (eq? (world-get-cell world pos) 'empty))

(define (world-cell-creature? world pos)
  (let* ((p (world-wrap-position world pos))
         (px (pos-x p))
         (py (pos-y p))
         (v (array-ref (world-cells world) px py)))
    (and (entity? v)
         (creature? (entity-value v)))))

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
             (make-entity name creature)
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
          (hash-set! (world-creatures world) (entity-name nc) new-pos)))))

;; todo
;;
;; adding/removing entities is all gonna look the same... smoosh them
;; all together?

(define (world-remove-creature world pos)
  (let* ((p (world-wrap-position world pos))
         (x (pos-x p))
         (y (pos-y p))
         (c (array-ref (world-cells world) x y)))
    (if (boolean? c) #f
        (begin
          (array-set! (world-cells world) #f x y)
          ;; fixme remove hash
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



(define world-add-rune
  (case-lambda
   ((world pos rune name)
    (let ((p (world-wrap-position world pos)))
      (when (world-cell-empty? world p)
        (world-cell-set! world rune pos)
        (hash-set! (world-runes world) name p))))
   ((world pos rune)
    (world-add-rune world pos rune (gensym "anon-rune-")))))

(define (world-remove-rune world name)
  (call/ec
   (λ (ret)
     (let* ((rs (world-runes world))
            (p (hash-ref rs name)))
       (unless p
         (msg 'warn "tried to remove a rune that doesn't exist: ~a" name)
         (ret #f))
       (world-cell-set! world #f p)
       (hash-remove! rs name)
       #t))))

(define (world-find-rune world name)
  (hash-ref (world-runes world) name))

(test-case "add and remove a rune"
  (let* ((w (make-blank-world 3))
         (p (make-pos 1 1))             ; position for a rune
         (r 'rune)                      ; rune value
         (n 'that-rune))                ; rune name
    (assert-equal #f (world-find-rune w n))
    ;; add
    (world-add-rune w p r n)
    (assert-equal p (world-find-rune w n))
    (assert-equal r (world-get-cell w p))
    ;; remove
    (world-remove-rune w n)
    (assert-equal #f (world-find-rune w n))
    (assert-equal 'empty (world-get-cell w p))))

(test-case "flip rune"
  (let* ((size 5)
         (w (make-blank-world size))
         (p0 (make-pos 3 2))            ; player pos before
         (p1 (make-pos 3 4))            ; player pos after
         (r0 (make-pos 3 3))            ; rune pos
         ;; a function to flip a position vertically about rune
         (f (cute pos-map-components <> (λ (x y) (make-pos x (- (* 2 (pos-y r0)) y)))))
         (walls (list (make-pos 2 2)
                      (make-pos 2 3)
                      (make-pos 2 4)
                      (make-pos 3 4)
                      (make-pos 4 4)
                      (make-pos 4 3)
                      (make-pos 4 2)))
         (walls-after (map f walls)))
    (world-spawn-creature w p0 'wizard 'player)
    (for-each (λ (p) (world-add-wall w p)) walls)
    (world-add-rune w r0 #\v)
    (for-each (λ (pos) (assert-equal 'wall (world-get-cell w pos)))
              walls-after)
    (assert-equal 'wizard (world-get-cell w p1))
    (assert-equal p1 (world-find-creature w 'player))))
