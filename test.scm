(define-module (test)
  #:use-module (system vm trace)
  #:re-export
  (call-with-trace)
  #:export
  (test-case
   assert-equal
   trc))

;; todo
;; assert equal doesn't work all that well,
;; what if it's in a method you call multiple times?
;; 1 - should throw
;; 2 - test-case can catch & report with backtrace
;; this means the test case doesn't progress beyond first error, good!

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

     ;; (unless (and (defined? 'inhibit-tests?) inhibit-tests?)
     ;;   (format #t "running test: ~a\n" name)
     ;;   body body* ...)

     (begin
       (format #t "running test: ~a\n" name)
       body body* ...)
     )))

(define-syntax trc
  (syntax-rules ()
    ((trc exp exp* ...)
     (call-with-trace
      (λ ()
        exp exp* ...)
      #:width 300))))
