(define-module (loader)
  #:use-module (srfi srfi-1)
  #:use-module (test)
  #:export
  (read-txt-to-array))

;; Read STR into an array of character and reverse the vertical-ordering.
(define (read-txt-to-array str)
  (let* ((lines (string-split str #\linefeed))
         (max-length (reduce max 1 (map string-length lines)))
         (pad (lambda (str) (string-pad-right str max-length #\space)))
         (padded (map pad lines)))
    (list->array 2 (reverse (map string->list padded)))))

(test "simple read"
      array-equal?
      (list->array 2 '((#\e #\f #\space)
                       (#\d #\space #\space)
                       (#\a #\b #\c)))
      (read-txt-to-array "abc\nd\nef"))
