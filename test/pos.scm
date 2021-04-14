(define-module (test pos)
  #:use-module (srfi srfi-1)
  #:use-module (util test)
  #:use-module (runes pos)
  #:export
  (all))

(test-case tangle
  "tangled rectangles"
  (assert-equal '() (rectangle-tangle '() #f #f))
  (let ((a (cons 'a (make-rectangle 0 1 0 1)))
        (b (cons 'b (make-rectangle 2 3 2 3)))
        (c (cons 'c (make-rectangle 3 4 3 4))))
    (assert-equal (list a)
                  (rectangle-tangle (list a b c)
                                    cdr
                                    (make-pos 0 0)))
    (assert-equal (list c b)
                  (rectangle-tangle (list a b c)
                                    cdr
                                    (make-pos 2 2)))))

(define (all)
  (tangle))
