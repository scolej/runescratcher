(define-module (util log)
  #:use-module (srfi srfi-28)
  #:export
  (msg))

(define (msg str . args)
  (display (apply format str args))
  (newline))
