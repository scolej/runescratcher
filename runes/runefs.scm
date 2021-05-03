(define-module (runes runefs)
  #:use-module (srfi srfi-26)
  #:use-module (runes pos)
  #:export
  (flipv
   fliph))

;; Make a vertical mirror function centred at CY.
(define (flipv cy)
  (cut pos-map-components <>
       (λ (x y)
         (make-pos x (- (* 2 cy) y)))))

;; Make a horizontal mirror function centred at CX.
(define (fliph cx)
  (cut pos-map-components <>
       (λ (x y)
         (make-pos (- (* 2 cx) x) y))))
