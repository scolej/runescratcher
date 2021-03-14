(use-modules
 (ice-9 match)
 (srfi srfi-9)
 (ncurses curses)
 (world))

(define world (blank-world 15))
(world-spawn-creature world (make-pos 0 0) 'player)
(world-spawn-creature world (make-pos 3 3) 'monster)
(world-set-cell world (make-pos 5 5) #t)
(world-set-cell world (make-pos 5 4) #t)
(world-set-cell world (make-pos 5 3) #t)

;;

(define stdscr (initscr))

(noecho!)
(cbreak!)
(keypad! stdscr #t)
(curs-set 0)

(define (draw)
  (erase stdscr)
  (match-let
      (((h w) (getmaxyx stdscr)))
    (do ((x 0 (1+ x))) ((= x w))
      (do ((y 0 (1+ y))) ((= y h))
        (let* ((v (world-get-cell world (make-pos x y)))
               (c (cond
                   ((boolean? v) (if v "#" #f))
                   ((eq? 'player v) "@")
                   (#t "?"))))
          (when c (addstr stdscr c #:x x #:y y))))))
  (refresh stdscr))

(define (go)
  (draw)
  (let ((c (getch stdscr)))
    ;; TODO-NEXT character movement, how to track player in world?
    ;; (cond
    ;;  ((eqv? c KEY_LEFT)
    ;;   (pos-update-x! char-pos (lambda (x) (- x 1))))
    ;;  ((eqv? c KEY_RIGHT)
    ;;   (pos-update-x! char-pos (lambda (x) (+ x 1))))
    ;;  ((eqv? c KEY_UP)
    ;;   (pos-update-y! char-pos (lambda (y) (- y 1))))
    ;;  ((eqv? c KEY_DOWN)
    ;;   (pos-update-y! char-pos (lambda (y) (+ y 1)))))
    (unless (eqv? c #\q) (go))))

(go)
(endwin)
