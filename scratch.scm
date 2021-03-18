(use-modules
 (ice-9 control)
 (rnrs base))

;; you dill, put the data in the array!

(define (hash-find-value table value)
  (%
   (abort 1)
   (lambda (k x) x)))

(let ((table (make-hash-table 1)))
  (hash-set! table 'bob (cons 1 2))
  (let ((v (hash-find-value table (cons 1 2))))
    (display v) (newline)
    (assert
     (eq? 'bob v))))
