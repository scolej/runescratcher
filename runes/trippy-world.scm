;;; Trippy World
;;;
;;; A world where arbitrary transformations on positions are possible.
;;;
;;; A "transformation" on a region may be added. Within this region,
;;; all positions are subject to provided function mapping from a
;;; "world" position to a "true" position.

(define-module (runes trippy-world)
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
  (make-rectangle l r t b)
  rectangle?
  (l rectangle-left)
  (r rectangle-right)
  (t rectangle-top)
  (b rectangle-bottom))

(define-record-type <transform>
  (make-transform rect f)
  transform?
  (rect transform-rect)
  (f transform-function))

(define-record-type <trippy-world>
  (make-trippy-world) trippy-world?
  (world base-world set-base-world!)
  (transforms get-transforms set-transforms!))

(define (make-world-empty size)
  (let ((w (make-trippy-world)))
    (set-base-world! w (base:make-world-empty size))
    (set-transforms! w (make-hash-table))
    w))

(define (world-add-wall world pos)
  (base:world-add-wall (base-world world) pos))

(define (world-cell-get world pos)
  (base:world-cell-get (base-world world) pos))

;; Adds the provided transform to the world.
;; Returns a symbol which can be used to refer to this transform later.
(define (world-add-transform world rect f)
  (let ((ts (get-transforms world))
        (sym (gensym "transform-"))
        (t (make-transform rect f)))
    (hash-table-set! ts  sym t)
    sym))
