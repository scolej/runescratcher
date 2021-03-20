(define-module (test)
  #:export (test))

(define-syntax test
  (syntax-rules ()
    ((test msg exp act) (test msg equal? exp act))
    ((test msg compare exp act)
     (unless (equal? exp act)
       (let ((loc (current-source-location)))
         (let ((f (assq-ref loc 'filename))
               (l (assq-ref loc 'line))
               (c (assq-ref loc 'column)))
           (format #t (string-join
                       '("--- fail! ---"
                         "~a:~a:~a"
                         "~a"
                         "expected: ~a"
                         "  actual: ~a")
                       "\n" 'suffix)
                   f (1+ l) c
                   msg exp act)))))))
