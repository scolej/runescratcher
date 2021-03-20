(use-modules
 (ice-9 match)
 (srfi srfi-9)
 (ncurses curses)
 (world))

(define world (make-world-from-file "map.txt"))
;; (define world (blank-world 300))
(world-spawn-creature world (make-pos 0 0) 'wizard 'player)
(world-spawn-creature world (make-pos 3 3) 'monster)
(world-add-wall world (make-pos 5 5))
(world-add-wall world (make-pos 5 4))
(world-add-wall world (make-pos 5 3))

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
                   ((eq? 'wizard v) "@")
                   (#t "?"))))
          (when c (addstr stdscr c #:x x #:y (- h y 1)))))))
  (refresh stdscr))

(define (go)
  (draw)
  (let ((c (getch stdscr)))
    ;; TODO-NEXT character movement, how to track player in world?
    (cond
     ((eqv? c KEY_LEFT)
      (world-move-creature world 'player 'west))
     ((eqv? c KEY_RIGHT)
      (world-move-creature world 'player 'east))
     ((eqv? c KEY_UP)
      (world-move-creature world 'player 'north))
     ((eqv? c KEY_DOWN)
      (world-move-creature world 'player 'south)))
    (unless (eqv? c #\q) (go))))

(with-error-to-file "err.log"
  (lambda ()
    (with-output-to-file "out.log"
      (lambda ()
        (go)
        (endwin)))))
