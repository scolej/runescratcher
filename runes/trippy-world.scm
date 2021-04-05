;;; Trippy World
;;;
;;; A world where arbitrary transformations on positions are possible.
;;;
;;; A "transformation" on a region may be added. Within this region,
;;; all positions are subject to provided function mapping from a
;;; "world" position to a "true" position.

(define-module (runes trippy-world)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (runes pos)
  #:use-module ((runes world) #:prefix base:)
  #:export
  (make-world-empty
   make-world-from-port
   world-cell-get
   world-cell-empty
   world-move
   world-find
   world-remove
   world-spawn
   world-add-wall
   world-add-transform
   make-rectangle
   rectangle?
   rectangle-left
   rectangle-right
   rectangle-top
   rectangle-bottom))

(define-record-type <rectangle>
  (make-rectangle l r b t)
  rectangle?
  (l rectangle-left)
  (r rectangle-right)
  (b rectangle-bottom)
  (t rectangle-top))

(define (rectangle-contains rect pos)
  (let ((x (pos-x pos))
        (y (pos-y pos))
        (l (rectangle-left rect))
        (r (rectangle-right rect))
        (b (rectangle-bottom rect))
        (t (rectangle-top rect)))
    (and (<= l x r)
         (<= b y t))))

(define-record-type <transform>
  (make-transform rect f)
  transform?
  (rect transform-rect)
  (f transform-function))

(define-record-type <trippy-world>
  (make-trippy-world) trippy-world?
  (world base-world set-base-world!)
  (transforms get-transforms set-transforms!))

(define (world->true world pos)
  (let* ((ts (hash-table-values (get-transforms world)))
         (rel (filter
               (λ (t)
                 (rectangle-contains (transform-rect t) pos))
               ts))
         (fs (map transform-function rel)))
    ((apply compose identity fs) pos)))

(define (true->world world pos)
  ;; fixme wrong
  (world->true world pos))

(define (make-world-empty size)
  (let ((w (make-trippy-world)))
    (set-base-world! w (base:make-world-empty size))
    (set-transforms! w (make-hash-table))
    w))

(define (world-add-wall world pos)
  (base:world-add-wall (base-world world) pos))

(define (world-cell-get world pos)
  (base:world-cell-get
   (base-world world) (world->true world pos)))

;; Adds the provided transform to the world.
;; Returns a symbol which can be used to refer to this transform later.
(define (world-add-transform world rect f)
  (let ((ts (get-transforms world))
        (sym (gensym "transform-"))
        (t (make-transform rect f)))
    (hash-table-set! ts  sym t)
    sym))

(define world-spawn
  (case-lambda
   ((world pos value name)
    (base:world-spawn
     (base-world world) (world->true world pos) value name))
   ((world pos value)
    (base:world-spawn
     (base-world world) (world->true world pos) value))))

;; fixme spawn & move
;; silly to branch twice with the same logic,
;; should provide API which is straight to the point

(define (world-move world what move)
  (let* ((pos0
          (cond
           ((pos? what) what)
           ((symbol? what) (world-find world what))
           (#t (error))))
         (pos1
          (cond
           ((pos? move) move)
           ((symbol? move) (relative-pos pos0 move))
           (#t (error)))))
    (base:world-move
     (base-world world)
     (world->true world pos0)
     (world->true world pos1))))

(define (world-find world name)
  (true->world
   world (base:world-find (base-world world) name)))
