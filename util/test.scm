(define-module (util test)
  #:use-module (system vm trace)
  #:use-module (ice-9 exceptions)
  #:export
  (assert-equal
   test-case
   trc))

(define-exception-type &test-exception &assertion-failure
  make-test-exception test-exception?
  (msg test-exception-message))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act) (assert-equal equal? exp act))
    ((_ compare exp act)
     (unless (equal? exp act)
       (let ((loc (current-source-location)))
         (let ((f (assq-ref loc 'filename))
               (l (assq-ref loc 'line))
               (c (assq-ref loc 'column)))
           (let ((msg (format #f (string-join
                                  '("--- fail! ---"
                                    "~a:~a:~a"
                                    "expected: ~a"
                                    "  actual: ~a")
                                  "\n" 'suffix)
                              f (1+ l) c
                              exp act)))
             (raise-exception
              (make-test-exception msg)))))))))

(define (call-with-bt-exn thunk)
  (call-with-prompt 'err
    (λ ()
      (with-exception-handler
       (λ (exn)
         (if (test-exception? exn)
             (abort-to-prompt 'err (make-stack #t 3) exn)
             (begin
               (newline)                ; fixme entangled formatting
               (raise-exception exn))))
       (λ () (thunk) #t)))
    (λ (c bt exn)
      (display-backtrace bt (current-output-port))
      (display (test-exception-message exn))
      #f)))

(define-syntax test-case
  (syntax-rules ()
    ((_ name descr body body* ...)
     (define (name)
       (let ((loc (current-source-location)))
         (let ((f (assq-ref loc 'filename))
               (l (assq-ref loc 'line)))
           (format #t "test: ~a (~a:~a)\n" descr f (1+ l))))
       (call-with-bt-exn (λ () body body* ...))))))

;; Trace all calls within body of this expression.
(define-syntax trc
  (syntax-rules ()
    ((trc exp exp* ...)
     (call-with-trace
      (λ ()
        exp exp* ...)
      #:width 300))))
