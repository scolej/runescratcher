;;; Grid world
;;;
;;; Models a world on a grid. Each cell is one of:
;;;
;;; - an empty cell
;;; - a wall
;;; - an entity with a name
;;;
;;; There may be only one entity per cell.
;;;
;;; An entity may not coincide with a wall.
;;;
;;; Anything out of bounds is a wall.

(define-module (runes world)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (util log)
  #:use-module (runes pos)
  #:export
  (make-world-empty
   make-world-from-port
   world-cell-get
   world-cell-empty
   world-move
   world-find
   world-remove
   world-spawn
   world-add-wall))

;; An entity is an object in the world decorated with a name. A name
;; is just a unique symbol which provides a way to refer to the
;; entity.
(define-record-type <entity>
  (make-entity name position value)
  entity?
  ;; Entity name. Any symbol.
  (name entity-name)
  ;; Entity position (no transforms), or #f.
  (position entity-pos entity-set-pos!)
  ;; Entity value. Anything at all.
  (value entity-value))



(define-record-type <world>
  (make-world-raw)
  world?
  ;; Grid of cells.
  (cells world-cells world-set-cells!)
  ;; Hash of entities. Key is entity name.
  (entities world-entities world-set-entities!))

;; Makes a new world, a square of the given size, with nothing in it.
(define (make-world-empty size)
  (let ((w (make-world-raw)))
    (world-set-cells! w (make-array #f size size))
    (world-set-entities! w (make-hash-table))
    w))

;; Set the value of the cell at POS to VAL.
(define (cell-set! world val pos)
  (array-set! (world-cells world) val (pos-x pos) (pos-y pos)))

;; Safely access the cells array and return the raw value.
(define (cell-get world pos)
  (let ((arr (world-cells world))
        (x (pos-x pos))
        (y (pos-y pos)))
    (match-let
        (((w h) (array-dimensions arr)))
      (if (and (< -1 x w)
               (< -1 y h))
          (array-ref arr x y)
          'wall))))

;; Get whatever is in the WORLD at given POS.
(define (world-cell-get world pos)
  (let ((v (cell-get world pos)))
    (cond
     ((entity? v) (entity-value v))
     ((eq? v #f) 'empty)
     (#t v))))

;; Is the cell at world empty?
(define (world-cell-empty? world pos)
  (eq? (world-cell-get world pos) 'empty))

;; Introduce a new entity into world at given
;; position, if there's space available. Returns #t if
;; successful, otherwise #f.
(define world-spawn
  (case-lambda
   ;; a new named entity
   ((world pos value name)
    (if (world-cell-empty? world pos)
        (let ((entity (make-entity name pos value)))
          (cell-set! world entity pos)
          (hash-table-set! (world-entities world) name entity)
          #t)
        #f))
   ;; anonymous value
   ((world pos value)
    (world-spawn
     world pos value (gensym "anon-entity-")))))

;; Find where in the world a named thing is.
;; Returns a position, or #f if no thing with name exists.
(define (world-find world name)
  (let ((ent (hash-table-ref/default (world-entities world) name #f)))
    (cond ((entity? ent) (entity-pos ent))
          (#t #f))))

;; Introduce a wall, as long as there's nothing there already.
(define (world-add-wall world pos)
  (if (world-cell-empty? world pos)
      (begin (cell-set! world 'wall pos) #t)
      #f))

;; Update world by moving something around.
;;
;; what - a name or a position
;; move - a direction or a new position
;;
;; The move only succeeds if the new position is empty.
(define (world-move world what move)
  (let* ((pos (cond ((symbol? what) (world-find world what))
                    ((pos? what) what)
                    (#t (error))))
         (new-pos (cond ((symbol? move) (relative-pos pos move))
                        ((pos? move) move)
                        (#t (error)))))
    (if (world-cell-empty? world new-pos)
      (let ((thing (cell-get world pos)))
        (cell-set! world #f pos)
        (cell-set! world thing new-pos)
        (when (entity? thing)
          (entity-set-pos!
           (hash-table-ref (world-entities world) (entity-name thing))
           new-pos))
        #t)
      #f)))

;; Remove whatever is at position, leaving an empty cell.
(define (world-remove world pos)
  (let ((thing (cell-get world pos)))
    (when (entity? thing)
      (hash-table-delete! (world-entities world) (entity-name thing)))
    (cell-set! world #f pos)))



;; Returns an array suitable for use as world cells by
;; interpreting characters in STR.
(define (read-array str)
  (define (world-read-char char)
    (match char
      (#\space #f)
      (#\# 'wall)
      (_ #f)))
  (define (read-txt-to-array str)
    (let* ((lines (filter (negate string-null?) (string-split str #\linefeed)))
           (max-length (reduce max 1 (map string-length lines)))
           (pad (λ (str) (string-pad-right str max-length #\space)))
           (padded (map pad lines)))
      (list->array 2 (reverse (map string->list padded)))))
  (let* ((src (read-txt-to-array str))
         (result (apply make-array #f (reverse (array-dimensions src)))))
    (array-index-map!
     result
     (λ (i j)
       (world-read-char
        (array-ref src j i))))
    result))

;; Construct a new world by reading characters from PORT.
;; Spaces are empty cells.
;; Hashes are walls.
;; The world is as wide as the longest line;
;; shorter lines are padded with empty cells.
(define (make-world-from-port port)
  (let* ((str (get-string-all port))
         (cells (read-array str))
         (w (make-world-raw)))
    (world-set-cells! w cells)
    (world-set-entities! w (make-hash-table))
    w))
