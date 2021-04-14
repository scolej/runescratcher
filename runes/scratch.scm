(use-modules
 (srfi srfi-1)
 (util test)
 (runes pos))

;; OBJS is a list of things which can be mapped to a rectangle using RECTF.
;; Finds the set of OBJS whose rectangles intersect with any other
;; rectangle which eventually contains with POS.
(define (rectangle-tangle objs rectf pos)
  (let go ((rem objs)
           (res '()))
    (if (null? rem) (reverse res)
        (let* ((o (car rem))
               (ro (rectf o)))
          (go
           (cdr rem)
           (if (or
                (rectangle-contains ro pos)
                (any (λ (oo) (rectangles-intersect (rectf oo) ro))
                     res))
               (cons o res)
               res))))))

(test-case tangle-1
  ""
  (assert-equal '() (rectangle-tangle '() #f #f))
  (let ((a (cons 'a (make-rectangle 0 1 0 1)))
        (b (cons 'b (make-rectangle 2 3 2 3)))
        (c (cons 'c (make-rectangle 3 4 3 4))))
    (assert-equal (list a)
                  (rectangle-tangle (list a b c)
                                    cdr
                                    (make-pos 0 0)))
    (assert-equal (list b c)
                  (rectangle-tangle (list a b c)
                                    cdr
                                    (make-pos 2 2)))))

(tangle-1)
