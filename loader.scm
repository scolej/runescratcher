(define-module (loader)
  #:use-module (srfi srfi-64)
  #:export
  (read-txt-to-array))

(define (read-txt-to-array str)
  (list->array
   2
   (map string->list
        (string-split str #\linefeed))))

(test-begin "file loading")

(test-assert "simple load"
  (array-equal?
   (list->array 2 '((#\a #\b #\c)
                    (#\d #f #f)
                    (#\e #\f #f)))
   (read-txt-to-array "abc\nd\nef")))


(test-end "file loading")
