(define-module (runes pos)
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
   pos?
   pos-x
   pos-y
   pos-map-components
   relative-pos))

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
