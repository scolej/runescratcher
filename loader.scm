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

(define (world-read-char char)
  (match char
    (#\space #f)
    (#\# #t)
    (_ #f)))

;; Returns an array suitable for use as world cells by
;; interpreting characters in the array SRC.
(define (world-read-array src)
  (let ((result (apply make-array #f (array-dimensions src))))
    (array-map! result world-read-char src)))

(test "read & interpret"
      array-equal?
      (list->array 2 '((#t #t #t)
                       (#\d #\space #\space)
                       (#\a #\b #\c)))
      (worl-read-array
       (read-txt-to-array "###\n# #\n###")))
