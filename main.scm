(use-modules
 (ice-9 match)
 (srfi srfi-9)
 (ncurses curses)
 (runes pos)
 (runes trippy-world)
 (runes game))

(define world (call-with-input-file "map2.txt" make-world-from-port))

(world-spawn world (make-pos 25 10) 'wizard 'player)
(world-spawn world (make-pos 3 3) 'monster)
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
          (let* ((v (world-cell-get world (make-pos x y)))
                 (c (cond
                      ((eq? v 'wall) "#")
                      ((eq? v 'wizard) "@")
                      ((eq? v 'empty) " ")
                      ((rune? v)
                       (cons 'red (case (rune-kind v)
                                    ((flip) "v")
                                    ((fliph) "h")
                                    (else "?"))))
                      (else "?"))))
            (cond
             ((string? c) (addstr stdscr c #:x x #:y (- h y 1)))
             ((pair? c) (draw-with-colour (cdr c) (car c)))))))))
  (refresh stdscr))

(define (go)
  (draw)
  (let ((c (getch stdscr)))
    (cond
     ((eqv? c KEY_LEFT) (game-input game 'left))
     ((eqv? c KEY_RIGHT) (game-input game 'right))
     ((eqv? c KEY_UP) (game-input game 'up))
     ((eqv? c KEY_DOWN) (game-input game 'down))
     ((eqv? c KEY_END) (game-input game 'escape))
     ((and (char? c) (not (eq? c #\q)))
      (game-input game (symbol c))))
    (unless (eqv? c #\q) (go))))

(with-exception-handler
 (λ (exn)
   (with-output-to-file "exn.txt"
     (λ ()
       (format #t "Error: ~a\n\nTrace\n" exn)
       (backtrace)
       (exit 1))))
 go)

(endwin)
