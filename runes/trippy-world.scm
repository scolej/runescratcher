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
  #:use-module (srfi srfi-26)
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
   world-remove-transform))

(define-record-type <transform>
  (make-transform i rect f fi)
  transform?
  (i transform-index)
  (rect transform-rect)
  (f transform-function)
  (fi transform-function-inverse))

(define-record-type <trippy-world>
  (make-trippy-world) trippy-world?
  (world base-world set-base-world!)
  (transforms get-transforms set-transforms!))

;; Finds all transforms in WORLD which might affect POS.
;; Results are sorted earliest transform first.
(define (relevant-transforms world pos)
  (sort
   (rectangle-tangle
    (hash-table-values (get-transforms world))
    transform-rect
    pos)
   ;; todo sort-by? compare?
   (λ (a b)
     (< (transform-index a)
        (transform-index b)))))

;; Apply the functions FS to X.
;; First element of F is applied last.
;; fixme recurse to be faster?
(define (chain fs x)
  ((apply compose identity fs) x))

;; Apply a transform T to POS if the area-of-effect contains POS.
(define (apply-transform t pos)
  (if (rectangle-contains (transform-rect t) pos)
      ((transform-function t) pos)
      pos))

;; Apply transform T's inverse transform to POS if the area-of-effect
;; contains POS.
(define (apply-transform-inverse t pos)
  (if (rectangle-contains (transform-rect t) pos)
      ((transform-function-inverse t) pos)
      pos))

(define (world->true world pos)
  (let* ((rel (relevant-transforms world pos))
         (g (λ (t) (cut apply-transform t <>)))
         (fs (map g rel)))
    (display "w->t ")(display (length fs)) (newline)
    (chain fs pos)))

(define (true->world world pos)
  (let* ((rel (reverse (relevant-transforms world pos)))
         (g (λ (t) (cut apply-transform-inverse t <>)))
         (fs (map g rel)))
    (display "t->w ") (display (length fs)) (newline)
    (chain fs pos)))

(define (make-world-empty size)
  (let ((w (make-trippy-world)))
    (set-base-world! w (base:make-world-empty size))
    (set-transforms! w (make-hash-table))
    w))

(define (make-world-from-port port)
  (let ((w (make-trippy-world)))
    (set-base-world! w (base:make-world-from-port port))
    (set-transforms! w (make-hash-table))
    w))

(define (world-add-wall world pos)
  (base:world-add-wall (base-world world) pos))

(define (world-cell-get world pos)
  (base:world-cell-get
   (base-world world) (world->true world pos)))

;; fixme parallelism
(define transform-ticker-next
  (let ((ticker 0))
    (λ ()
      (set! ticker (1+ ticker))
      ticker)))

;; Adds the provided transform to the world.
(define (world-add-transform world rect f fi sym)
  (let ((ts (get-transforms world))
        (t (make-transform
            (transform-ticker-next)
            rect f fi)))
    (hash-table-set! ts sym t)))

(define (world-remove-transform world t)
  (hash-table-delete! (get-transforms world) t))

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
   world
   (base:world-find (base-world world) name)))
