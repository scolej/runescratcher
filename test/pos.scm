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
        (c (cons 'c (make-rectangle 3 4 3 4)))
        (d (cons 'd (make-rectangle 4 5 4 5))))
    (let ((g (λ (rs pos)
               (sort
                (rectangle-tangle rs cdr pos)
                (λ (a b)
                  (string< (symbol->string (car a))
                           (symbol->string (car b))))))))
      (assert-equal (list a)
                    (g (list a b c) (make-pos 0 0)))
      (assert-equal (list b c)
                    (g (list a b c) (make-pos 2 2)))
      (assert-equal (list b c d)
                    (g (list d c b a) (make-pos 2 2))))))

(define (all)
  (tangle))
