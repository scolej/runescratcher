(define-module (test)
  #:export
  (test-case
   assert-equal))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act) (assert-equal equal? exp act))
    ((_ compare exp act)
     (unless (equal? exp act)
       (let ((loc (current-source-location)))
         (let ((f (assq-ref loc 'filename))
               (l (assq-ref loc 'line))
               (c (assq-ref loc 'column)))
           (format #t (string-join
                       '("--- fail! ---"
                         "~a:~a:~a"
                         "expected: ~a"
                         "  actual: ~a")
                       "\n" 'suffix)
                   f (1+ l) c
                   exp act)))))))

(define-syntax test-case
  (syntax-rules ()
    ((_ name body body* ...)

     ;; doesn't seem to work
     ;; (eval-when (compile)
     ;;   body body* ...)

     (unless (and (defined? 'inhibit-tests?) inhibit-tests?)
       (format #t "running test: ~a\n" name)
       body body* ...)

     ;; (begin
     ;;   body body* ...)
     )))
