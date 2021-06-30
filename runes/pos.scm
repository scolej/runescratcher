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
   relative-pos
   make-rectangle
   rectangle?
   rectangle-contains
   rectangles-intersect
   rectangle-left
   rectangle-right
   rectangle-bottom
   rectangle-top
   rectangle-tangle))

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

(define (rectangles-intersect rect1 rect2)
  (let ((l1 (rectangle-left rect1))
        (r1 (rectangle-right rect1))
        (b1 (rectangle-bottom rect1))
        (t1 (rectangle-top rect1))
        (l2 (rectangle-left rect2))
        (r2 (rectangle-right rect2))
        (b2 (rectangle-bottom rect2))
        (t2 (rectangle-top rect2)))
    (and (<= l1 r2)
         (>= r1 l2)
         (<= b1 t2)
         (>= t1 b2))))

;; OBJS is a list whose elements can be mapped to a rectangle using RECTF.
;; Finds the set of OBJS whose rectangles intersect with any other
;; rectangle which eventually contains the seed position POS. Order of
;; results is unspecified.
(define (rectangle-tangle objs rectf pos)
  ;; Is object O1 relevant to the current set of relevent object OS?
  (define (relevant? o1 os)
    ;; Do the rectangles obtained from O1 and O2 intersect?
    (define (intersects? o1 o2)
      (rectangles-intersect (rectf o1) (rectf o2)))
    ;; Does the rectangle obtained from O contain the seed position?
    (define (contains-pos? o)
      (rectangle-contains (rectf o) pos))
    (or (contains-pos? o1)
        (any (λ (o2) (intersects? o1 o2)) os)))
  (let go ((rem objs) (res '()))
    (match rem
      (() res)
      ((o  . rem)
       (if (relevant? o res)
           (let ((res (cons o res)))
             (go (lset-difference eq? objs res) res))
           (go rem res))))))
