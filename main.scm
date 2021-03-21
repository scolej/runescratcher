(use-modules
 (ice-9 match)
 (srfi srfi-9)
 (ncurses curses)
 (world)
 (game))

(define world (make-world-from-file "map.txt"))
;; (define world (blank-world 300))
(world-spawn-creature world (make-pos 0 0) 'wizard 'player)
(world-spawn-creature world (make-pos 3 3) 'monster)
(world-add-wall world (make-pos 5 5))
(world-add-wall world (make-pos 5 4))
(world-add-wall world (make-pos 5 3))

(define game (make-game-with-world world))

;;

(define stdscr (initscr))

(noecho!)
(cbreak!)
(keypad! stdscr #t)
(curs-set 0)
(notimeout! stdscr #t)

(start-color!)
(assume-default-colors 0 -1)
(init-pair! 1 COLOR_RED -1)
(init-pair! 2 COLOR_BLUE -1)

(define (draw)
  (erase stdscr)
  (match-let
      (((h w) (getmaxyx stdscr)))
    (do ((x 0 (1+ x))) ((= x w))
      (do ((y 0 (1+ y))) ((= y h))
        (let ((draw-with-colour
               (lambda (str c)
                 (let ((ci (case c
                             ((red) 1)
                             ((blue) 2))))
                   (attr-on! stdscr (color-pair ci))
                   (addstr stdscr str #:x x #:y (- h y 1))
                   (attr-off! stdscr (color-pair ci))))))
          (let* ((v (world-get-cell world (make-pos x y)))
                 (c (match v
                      ('wall "#")
                      ('wizard "@")
                      ('empty " ")
                      (('rune . r) (cons 'red (format #f "~s" r)))
                      (_ "?"))))
            (cond
             ((string? c) (addstr stdscr c #:x x #:y (- h y 1)))
             ((pair? c) (draw-with-colour (cdr c) (car c)))))))))
  (refresh stdscr))

(define (go)
  (draw)
  (let ((c (getch stdscr)))
    ;; TODO-NEXT character movement, how to track player in world?
    (cond
     ((eqv? c KEY_LEFT) (game-input game 'left))
     ((eqv? c KEY_RIGHT) (game-input game 'right))
     ((eqv? c KEY_UP) (game-input game 'up))
     ((eqv? c KEY_DOWN) (game-input game 'down))
     ((eqv? c KEY_END) (game-input game 'escape))
     ((and (char? c) (not (eq? c #\q)))
      (game-input game (symbol c))))
    (unless (eqv? c #\q) (go))))

(go)
(endwin)
