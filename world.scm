(define-module (world)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-69)
  #:export
  (make-pos
   pos-x
   pos-y
   pos-map-components
   relative-pos
   ;; fixme expose this?
   entity?
   entity-name
   entity-value
   make-world
   make-blank-world
   world-read-array
   world-cell-get
   world-set-cells!
   world-set-runes!
   world-get-entity-value ; fixme :(
   world-move-creature
   world-spawn-creature
   world-remove-creature
   world-remove-rune
   world-find-creature
   world-find-rune
   world-add-wall
   world-add-rune
   make-blank-world
   make-world-from-file
   make-rune
   rune?
   run-tests))

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

(define (rune-apply rune pos)
  (let* ((r (rune-pos rune))
         (rx (pos-x r))
         (ry (pos-y r))
         (px (pos-x pos))
         (py (pos-y pos))
         (range 2)
         (in-range (and (< (abs (- px rx)) range)
                        (< (abs (- py ry)) range))))
    (if (not in-range)
        pos
        (make-pos px (- (* 2 ry) py)))))

;; Transform a position with all the impacting rune effects.
(define (rune-transform world pos)
  (let* ((rune-ps (hash-table-values (world-runes world)))
         (all (map (compose entity-value
                            (cut world-cell-get-prim world <>))
                   rune-ps)))
    (fold rune-apply pos rel)))

;; Set the value of the cell at POS to VAL.
(define (world-cell-set! world val pos)
  (array-set! (world-cells world) val (pos-x pos) (pos-y pos)))

;; fixme
;;
;; filtering runes by position also broken, if runes can flip other runes about,
;; then a simple rectangle check is not enough. imagine that you're filtering by
;; aeo rect, they can compound the extend the area.
;;

;; Get whatever is in the WORLD at given POS.
(define (world-cell-get world pos)
  (let ((arr (world-cells world))
        (x (pos-x pos))
        (y (pos-y pos)))
    (match-let
        (((w h) (array-dimensions arr)))
      (if (and (< -1 x w)
               (< -1 y h))
          (let ((v (array-ref arr x y)))
            (cond
             ((eq? v #f) 'empty)
             (#t v)))
          #t))))

;; (define (world-cell-get world posit)
;;   (let ((pos (rune-transform world posit)))
;;     (let ((arr (world-cells world))
;;           (x (pos-x pos))
;;           (y (pos-y pos)))
;;       (match-let
;;         (((w h) (array-dimensions arr)))
;;         (if (and (< -1 x w)
;;                  (< -1 y h))
;;             (let ((v (array-ref arr x y)))
;;               (cond
;;                 ((eq? v #f) 'empty)
;;                 (#t v)))
;;             #t)))))



(define (world-get-entity-value world pos)
  (let ((e (world-cell-get world pos)))
    (if (entity? e) (entity-value e) #f)))



(define (world-cell-empty? world pos)
  (eq? (world-cell-get world pos) 'empty))

(define (world-cell-creature? world pos)
  (let ((v (world-cell-get world pos)))
    (and (entity? v)
         (creature? (entity-value v)))))

;; Introduce a new creature into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define world-spawn-creature
  (case-lambda
   ;; Allow naming the creature for later look-up.
   ((world pos creature name)
    (if (not (world-cell-empty? world pos)) #f
        (begin
          (world-cell-set! world (make-entity name creature) pos)
          (hash-table-set! (world-creatures world) name pos)
          #t)))
   ;; Anonymous creature.
   ((world pos creature)
    (world-spawn-creature
     world pos creature (gensym "anon-creature-")))))

(define (world-find-creature world name)
  (hash-table-ref/default (world-creatures world) name #f))

(define (world-add-wall world pos)
  (unless (world-cell-creature? world pos)
    (world-cell-set! world 'wall pos)))

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
    (when (and (world-cell-creature? world pos)
               (world-cell-empty? world new-pos))
      (let ((nc (world-cell-get world pos)))
        (world-cell-set! world #f pos)
        (world-cell-set! world nc new-pos)
        (hash-table-set! (world-creatures world)
                         (entity-name nc) new-pos)))))

;; todo
;;
;; adding/removing entities is all gonna look the same... smoosh them
;; all together?

(define (world-remove-creature world pos)
  (let ((c (world-cell-get world pos)))
    (if (boolean? c) #f
        (begin
          (world-cell-set! world #f pos)
          ;; fixme remove hash
          c))))



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
    (when (world-cell-empty? world pos)
      (world-cell-set! world (make-entity name rune) pos)
      (hash-table-set! (world-runes world) name pos)))
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
