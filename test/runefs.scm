(define-module (test runefs)
  #:use-module (runes pos)
  #:use-module (runes runefs)
  #:use-module (util test)
  #:export
  (all))

(test-case vert-flip
  "vertical flip"
  (let ((g (flipv 10)))
    (assert-equal (make-pos 3 9) (g (make-pos 3 11)))

    ;; flipv is its own inverse
    (for-each
     (λ (pos)
       (assert-equal pos (g (g pos))))
     (list (make-pos 0 0)
           (make-pos 0 10)
           (make-pos 10 0)
           (make-pos 3 9)
           (make-pos 3 11)))))

(test-case horiz-flip
  "horizontal flip"
  (let ((g (fliph 10)))
    (assert-equal (make-pos 9 3) (g (make-pos 11 3)))

    ;; fliph is its own inverse
    (for-each
     (λ (pos)
       (assert-equal pos (g (g pos))))
     (list (make-pos 0 0)
           (make-pos 0 10)
           (make-pos 10 0)
           (make-pos 3 9)
           (make-pos 3 11)))))

(define (all)
  (vert-flip)
  (horiz-flip))
