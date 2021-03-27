(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-69)
  #:use-module (test)
  #:export
  (make-pos
   pos-x
   pos-y
   relative-pos
   ;; fixme expose this?
   entity?
   entity-name
   entity-value
   world-get-cell
   world-get-entity-value ; fixme :(
   world-move-creature
   world-spawn-creature
   world-remove-creature
   world-find-creature
   world-add-wall
   world-add-rune
   make-blank-world
   make-world-from-file
   make-rune
   rune?))

;; todo - a sensible prelude



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
    (world-set-creatures! w (make-hash-table))
    (world-set-runes! w (make-hash-table))
    w))



(define-record-type <rune>
  (make-rune pos) rune?
  (pos rune-pos rune-set-pos!))

(define (rune-affects pos rune)
  (let* ((r (rune-pos rune))
         (rx (pos-x r))
         (ry (pos-y r))
         (px (pos-x pos))
         (py (pos-y pos)))
    (and (< (abs (- px rx)) 2)
         (< (abs (- py ry)) 2))))

(define (rune-apply rune pos)
  (let* ((r (rune-pos rune))
         (ry (pos-y r))
         (px (pos-x pos))
         (py (pos-y pos)))
    (make-pos px (- (* 2 ry) py))))

;; Transforms a world position to a valid index into the cells array.
;; - wrap around indices outside bounds
;; - transform positions inside runes' area-of-effect
;;
;; todo, worry about whether wrap or transform first makes a difference
(define (world->array world pos)
  (define (wrap pos)
    (match-let (((w h) (array-dimensions (world-cells world))))
      (let ((x (modulo (pos-x pos) w))
            (y (modulo (pos-y pos) h)))
        (make-pos x y))))
  (define (rune-transforms pos)
    (let* ((rune-ps (hash-table-values (world-runes world)))
           (all (map (compose entity-value (cut world-cell-get world <>)) rune-ps))
           (rel (filter (cut rune-affects pos <>) all)))
      (fold rune-apply pos rel)))
  ((compose wrap rune-transforms) pos))

;; fixme - how to reverse a rune?
;; do we say that simply applying the funtion to itself is enough?
(define (array->world world pos)
  (define (wrap pos)
    (match-let (((w h) (array-dimensions (world-cells world))))
      (let ((x (modulo (pos-x pos) w))
            (y (modulo (pos-y pos) h)))
        (make-pos x y))))
  (define (rune-transforms pos)
    (let* ((rune-ps (hash-table-values (world-runes world)))
           (all (map (compose entity-value (cut world-cell-get world <>)) rune-ps))
           (rel (filter (cut rune-affects pos <>) all)))
      (fold rune-apply pos rel)))
  ((compose wrap rune-transforms) pos))

;; Set the value of the cell at POS to VAL.
;; No checking, no position transform, no nothing.
;; Use with care.
(define (world-cell-set! world val pos)
  (array-set! (world-cells world) val (pos-x pos) (pos-y pos)))

;; Get whatever is in the world at given position
;; No checking, no position transform, no nothing.
;; Use with care.
(define (world-cell-get world pos)
  (let ((w (world-cells world))
        (x (pos-x pos))
        (y (pos-y pos)))
    (let ((v (array-ref w x y)))
      (cond
       ((eq? v #f) 'empty)
       (#t v)))))

;; Gets what's in the world at the provided position.
(define (world-get-cell world pos)
  (world-cell-get world (world->array world pos)))

(define (world-get-entity-value world pos)
  (let ((e (world-get-cell world pos)))
    (if (entity? e) (entity-value e) #f)))



(define (world-cell-empty? world pos)
  (eq? (world-get-cell world pos) 'empty))

(define (world-cell-creature? world pos)
  (let* ((p (world->array world pos))
         (v (world-cell-get world p)))
    (and (entity? v)
         (creature? (entity-value v)))))

;; Introduce a new creature into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define world-spawn-creature
  (case-lambda
   ;; Allow naming the creature for later look-up.
   ((world pos creature name)
    (let* ((p (world->array world pos)))
      (if (not (world-cell-empty? world p)) #f
          (begin
            (world-cell-set! world (make-entity name creature) p)
            (hash-table-set! (world-creatures world) name p)
            #t))))
   ;; Anonymous creature.
   ((world pos creature)
    (world-spawn-creature
     world pos creature (gensym "anon-creature-")))))

(define (world-find-creature world name)
  (array->world
   world
   (hash-table-ref/default (world-creatures world) name #f)))

(define (world-add-wall world pos)
  (let ((p (world->array world pos)))
    (unless (world-cell-creature? world p)
      (world-cell-set! world 'wall p))))

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
              move))
         (pos-arr (world->array world pos))
         (new-pos-arr (world->array world new-pos)))
    (when (and (world-cell-creature? world pos)
               (world-cell-empty? world new-pos))
      (let ((nc (world-cell-get world pos-arr)))
        (world-cell-set! world #f pos-arr)
        (world-cell-set! world nc new-pos-arr)
        (hash-table-set! (world-creatures world)
                         (entity-name nc) new-pos)))))

;; todo
;;
;; adding/removing entities is all gonna look the same... smoosh them
;; all together?

(define (world-remove-creature world pos)
  (let* ((p (world->array world pos))
         (c (world-cell-get world p)))
    (if (boolean? c) #f
        (begin
          (world-cell-set! world #f p)
          ;; fixme remove hash
          c))))

(test-case "moving creatures around the world"
  (let ((world (make-blank-world 10)))
    ;; there is nothing
    (assert-equal 'empty (world-get-cell world (make-pos 0 0)))
    ;; there is the player
    (world-spawn-creature world (make-pos 0 0) 'player 'bob)
    (assert-equal 'player (world-get-entity-value world (make-pos 0 0)))
    (assert-equal (make-pos 0 0) (world-find-creature world 'bob))
    ;; player moves north east
    (world-move-creature world (make-pos 0 0) (make-pos 1 1))
    (assert-equal 'empty (world-get-cell world (make-pos 0 0)))
    (assert-equal 'player (world-get-entity-value world (make-pos 1 1)))
    (assert-equal (make-pos 1 1) (world-find-creature world 'bob))
    ;; player moves east
    (world-move-creature world (make-pos 1 1) 'east)
    (assert-equal 'empty (world-get-cell world (make-pos 1 1)))
    (assert-equal 'player (world-get-entity-value world (make-pos 2 1)))
    (assert-equal (make-pos 2 1) (world-find-creature world 'bob))
    ;; player moves south
    (world-move-creature world 'bob 'south)
    (assert-equal 'empty (world-get-cell world (make-pos 2 1)))
    (assert-equal 'player (world-get-entity-value world (make-pos 2 0)))
    (assert-equal (make-pos 2 0) (world-find-creature world 'bob))
    ;; Move off left edge.
    (world-move-creature world 'bob (make-pos 0 0))
    (world-move-creature world 'bob 'west)
    ;; Move him to a known position.
    (world-move-creature world 'bob (make-pos 2 1))
    ;; world wraps
    (assert-equal 'player (world-get-entity-value world (make-pos 12 11)))
    (assert-equal 'player (world-get-entity-value world (make-pos -8 -9)))
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
    (world-set-runes! w (make-hash-table))
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
    (world-set-creatures! w (make-hash-table))
    (world-set-runes! w (make-hash-table))
    w))



(define world-add-rune
  (case-lambda
   ((world pos rune name)
    (let ((p (world->array world pos)))
      (when (world-cell-empty? world p)
        (world-cell-set! world (make-entity name rune) pos)
        (hash-table-set! (world-runes world) name p))))
   ((world pos rune)
    (world-add-rune world pos rune (gensym "anon-rune-")))))

(define (world-remove-rune world name)
  (call/ec
   (λ (ret)
     (let* ((rs (world-runes world))
            (p (hash-table-ref/default rs name #f)))
       (unless p
         (msg 'warn "tried to remove a rune that doesn't exist: ~a" name)
         (ret #f))
       (world-cell-set! world #f p)
       (hash-table-delete! rs name)
       #t))))

(define (world-find-rune world name)
  (hash-table-ref/default (world-runes world) name #f))

;; todo, ideas
;;
;; util to assert pos in cells & pos reported for entity are consistent & equal
;; to a given value

;; Assert that CREATURE with NAME is at POS in WORLD.
(define (assert-creature-position world creature name pos)
  (assert-equal creature (world-get-entity-value world pos))
  (assert-equal pos (world-find-creature world name)))

;; Assert that RUNE with NAME is at POS in WORLD.
(define (assert-rune-position world rune name pos)
  (assert-equal rune (world-get-entity-value world pos))
  (assert-equal pos (world-find-rune world name)))

(test-case "add and remove a rune"
  (let* ((w (make-blank-world 3))
         (p (make-pos 1 1))             ; position for a rune
         (r (make-rune p))              ; rune value
         (n 'that-rune))                ; rune name
    (assert-equal #f (world-find-rune w n))
    ;; add
    (world-add-rune w p r n)
    (assert-rune-position w r n p)
    ;; remove
    (world-remove-rune w n)
    (assert-equal #f (world-find-rune w n))
    (assert-equal 'empty (world-get-cell w p))))

(test-case "flip rune"
  (let* ((size 5)
         (w (make-blank-world size))
         (p0 (make-pos 2 1))            ; player pos before
         (r0 (make-pos 2 2))            ; rune pos
         (p1 (make-pos 2 3))            ; player pos after
         ;; a function to flip a position vertically about rune
         (f (cute pos-map-components <>
                  (λ (x y) (make-pos x (- (* 2 (pos-y r0)) y)))))
         (walls (list (make-pos 1 1)
                      (make-pos 1 2)
                      (make-pos 1 3)
                      (make-pos 2 3)
                      (make-pos 3 3)
                      (make-pos 3 2)
                      (make-pos 3 1)))
         (walls-after (map f walls)))
    (world-spawn-creature w p0 'wizard 'player)
    (for-each
     (λ (p)
       (world-add-wall w p))
     walls)
    (world-add-rune w r0 (make-rune r0))
    (for-each
     (λ (pos)
       (assert-equal 'wall (world-get-cell w pos)))
     walls-after)
    (assert-creature-position w 'wizard 'player p1)))
