;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Minesweeper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; === Start HW10 Solution Code ===
(require 2htdp/universe)
;; Exercise 2

;; A Content is one of:
;; - Nat
;; - #t
;; and represents either a count of the number of mines surrounding
;; a cell or a mine itself

(define CONTENT-BLANK 0)
(define CONTENT-MINE #t)

;; content-template : Content -> ???
(define (content-template c)
  (cond [(number? c) (... c ...)]
        [(boolean? c) ...]))

(define-struct hidden [con])
(define-struct visible [con])
(define-struct flagged [con])

;; A Cell is one of:
;; - (make-hidden Content)
;; - (make-visible Content)
;; - (make-flagged Content)
;; and represents either a hidden cell, a visible cell, or a flagged cell

(define CELL-H0 (make-hidden 0))
(define CELL-V0 (make-visible 0))
(define CELL-F0 (make-flagged 0))

(define CELL-H1 (make-hidden 1))
(define CELL-V1 (make-visible 1))
(define CELL-F1 (make-flagged 1))

(define CELL-HM (make-hidden #t))
(define CELL-VM (make-visible #t))
(define CELL-FM (make-flagged #t))

;; cell-template : Cell -> ???
(define (cell-template cell)
  (cond [(hidden? cell) (... (hidden-con cell) ...)]
        [(visible? cell) (... (visible-con cell) ...)]
        [(flagged? cell) (... (flagged-con cell) ...)]))

;; Exercise 3

;; A Board is a [List-of [List-of Cell]
;; and represents a grid of cells that make up a game board

(define BOARD-EMPTY '())
(define BOARD-SEMI-EMPTY '(() () () ()))
(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))
(define BOARD-3X3-MID (list (make-list 3 CELL-H1)
                            (list CELL-H1 CELL-HM CELL-H1)
                            (make-list 3 CELL-H1)))
(define BOARD-LOSE (list (list CELL-VM)))

;; board-template : Board -> ???
(define (board-template b)
  (cond [(empty? b) ...]
        [(cons? b) (... (row-template (first b))
                        (board-template (rest b)) ...)]))

;; row-template : [List-of Cell] -> ???
(define (row-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc) (... (cell-template (first loc))
                          (row-template (rest loc)) ...)]))

;; Exercise 4
(define-struct game [board rev?])
;; A Game is a (make-game Board Boolean)
;; and represents a game of Minesweeper with a board of cells and a flag that is
;; #t if the mouse is revealing cells and #f if it is flagging them

(define GAME-EMPTY (make-game BOARD-EMPTY #t))
(define GAME-2X2-T (make-game BOARD-2X2-BLANK #t))
(define GAME-2X2-F (make-game BOARD-2X2-BLANK #f))
(define GAME-3X3-T (make-game BOARD-3X3-MID #t))
(define GAME-3X3-F (make-game BOARD-3X3-MID #f))
(define GAME-LOSE (make-game BOARD-LOSE #t))

;; game-template : Game -> ???
(define (game-template g)
  (... (board-template (game-board g))
       (game-rev? g) ...))

;; Exercise 5
(require 2htdp/universe)
(require 2htdp/image)

;; mine-sweeper : Nat Nat -> Game
;; Play the minesweeper game with a square board of the given size and the
;; given number of mines
(define (mine-sweeper size mines)
  (mine-sweeper-from (make-game (generate-mine-board size mines) #t)))

;; mine-sweeper-from : Game -> Game
;; Play the minesweeper game with the given initial game state
(define (mine-sweeper-from g)
  (big-bang g
    [to-draw draw-game]
    [on-mouse process-mouse]
    [on-key change-mouse-state]
    [stop-when game-over? draw-game-over]))



;; There are a lot of board generation functions below. Please note that it was not
;; required that you generate a board in this way. It was fine to pass a game with a constant
;; board to mine-sweeper-from instead.


;; add-counts : Board -> Board
;; Add the correct count for each item on the given board
(check-expect (add-counts '()) '())
(check-expect
 (add-counts (list (make-list 3 CELL-H0)
                   (list CELL-H0 CELL-HM CELL-H0)
                   (make-list 3 CELL-H0)))
 BOARD-3X3-MID)
(define (add-counts b)
  (build-list (length b)
              (λ (row) (build-list (length b)
                                   (λ (col) (add-count-to-cell b row col))))))

;; add-count-to-cell : Board Nat Nat -> Cell
;; If the cell at the given location is a mine, leave it alone
;; If it is not a mine, count the mines around it and make that the count for the cell
(check-expect (add-count-to-cell BOARD-3X3-MID 0 0) CELL-H1)
(check-expect
 (add-count-to-cell
  (list (list CELL-HM CELL-HM) (list CELL-HM CELL-H0)) 1 1)
 (make-hidden 3))
(define (add-count-to-cell board row col)
  (local [(define cell (list-ref (list-ref board row) col))
          (define neighbor-posns (get-neighbor-indices row col (length board)))
          (define neighbor-cells
            (map (λ (p) (list-ref (list-ref board (posn-x p)) (posn-y p))) neighbor-posns))]
    (update-cell-count cell (length (filter mine-cell? neighbor-cells)))))

;; get-neighbor-indices : Nat Nat Nat -> [List-of Posn]
;; Get a list of the row/column indices of the neighbors of the cell with the given indices
(check-expect (get-neighbor-indices 0 0 0) '())
(check-expect
 (get-neighbor-indices 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (get-neighbor-indices 2 2 3)
 (list (make-posn 1 1) (make-posn 1 2) (make-posn 2 1)))
(check-expect
 (get-neighbor-indices 1 2 4)
 (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)
       (make-posn 1 1) (make-posn 1 3)
       (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)))
(define (get-neighbor-indices row col size)
  (local [(define can-add-left? (> size col 0))
          (define can-add-right? (< col (sub1 size)))
          (define can-add-up? (> size row 0))
          (define can-add-down? (< row (sub1 size)))]
    (append (if (and can-add-left? can-add-up?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-add-up? (list (make-posn (sub1 row) col)) '())
            (if (and can-add-right? can-add-up?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-add-left? (list (make-posn row (sub1 col))) '())
            (if can-add-right? (list (make-posn row (add1 col))) '())
            (if (and can-add-left? can-add-down?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add-down? (list (make-posn (add1 row) col)) '())
            (if (and can-add-right? can-add-down?) (list (make-posn (add1 row) (add1 col))) '()))))

;; update-cell-count : Cell Nat -> Cell
;; If this cell is a mine, leave it, otherwise update to the given count
(check-expect (update-cell-count CELL-HM 5) CELL-HM)
(check-expect (update-cell-count (make-visible 7) 2) (make-visible 2))
(check-expect (update-cell-count (make-flagged 0) 3) (make-flagged 3))
(define (update-cell-count cell new-count)
  (local [;; update-contents : Content -> Content
          ;; Update the contents if it is not a mine to be new-count
          (define (update-contents c)
            (if (boolean? c) c new-count))]
    (cond [(hidden? cell)
           (make-hidden (update-contents (hidden-con cell)))]
          [(visible? cell)
           (make-visible (update-contents (visible-con cell)))]
          [(flagged? cell)
           (make-flagged (update-contents (flagged-con cell)))])))

;; mine-cell? : Cell -> Boolean
;; Is this a mine cell?
(check-expect (mine-cell? CELL-HM) #t)
(check-expect (mine-cell? (make-visible 3)) #f)
(check-expect (mine-cell? (make-flagged 0)) #f)
(define (mine-cell? cell)
  (cond [(hidden? cell) (boolean? (hidden-con cell))]
        [(visible? cell) (boolean? (visible-con cell))]
        [(flagged? cell) (boolean? (flagged-con cell))]))

;; Exercise 6
;; change-mouse-state : Game KeyEvent -> Game
;; Change the state of the mouse if the user pressed the space bar
(check-expect (change-mouse-state GAME-EMPTY "x") GAME-EMPTY)
(check-expect (change-mouse-state GAME-2X2-F " ") GAME-2X2-T)
(define (change-mouse-state g key)
  (if (key=? key " ") (make-game (game-board g) (not (game-rev? g))) g))

;; Exercise 7
;; game-over? : Game -> Boolean
;; Did the user either win or lose?
(check-expect (game-over? GAME-LOSE) #t)
(check-expect (game-over? GAME-EMPTY) #t)
(check-expect (game-over? GAME-3X3-T) #f)
(define (game-over? g)
  (or (board-win? (game-board g))
      (board-lose? (game-board g))))

;; board-win? : Board -> Boolean
;; Did the user reveal all the non-mine squares?
(check-expect (board-win? BOARD-EMPTY) #t)
(check-expect (board-win? BOARD-3X3-MID) #f)
(check-expect (board-win? (make-list 2 (make-list 2 CELL-FM))) #t)
(define (board-win? board)
  (local [;; mine-or-visible? : Cell -> Boolean
          ;; Is the given cell either a mine or visible?
          (define (mine-or-visible? cell)
            (cond [(visible? cell) #t]
                  [(hidden?  cell) (boolean? (hidden-con cell))]
                  [(flagged? cell) (boolean? (flagged-con cell))]))]
    (andmap (λ (row) (andmap mine-or-visible? row)) board)))

;; board-lose? : Board -> Boolean
;; Is there any visible mine square on the board?
(check-expect (board-lose? BOARD-3X3-MID) #f)
(check-expect
 (board-lose?
  (list (list (make-hidden #t) (make-visible 0))
        (list (make-flagged #t) (make-visible #t))))
 #t)
(define (board-lose? board)
  (local [;; visible-mine? : Cell -> Boolean
          ;; Is the given cell a mine that is visible?
          (define (visible-mine? cell)
            (and (visible? cell) (boolean? (visible-con cell))))]
    (ormap (λ (row) (ormap visible-mine? row)) board)))

; === End HW 10 Solution Code ===


; == Drawing Fucntions ===


(define CELL-SIZE 32)
(define CELL-BACKGROUND (overlay (square CELL-SIZE "outline" "black")
                                 (square CELL-SIZE "solid" "light grey")))
; mine-size: Nat -> Image
; produces an image of a mine of the given size
(define (draw-mine size)
  (local [(define mine-pen (pen "black" (floor (* size 0.1)) "solid" "round" "bevel"))
          (define mine-size (* size 0.8))]
    (underlay/align/offset "center" "center"
                           (overlay (circle (* size 0.3)  "solid" "black")
                                    (line 0 mine-size mine-pen)
                                    (line mine-size 0 mine-pen)
                                    (line (* 0.707 mine-size) (* 0.707 mine-size) mine-pen)
                                    (line (* 0.707 mine-size) (* -0.707 mine-size) mine-pen))
                           (* -0.15 size)
                           (* -0.15 size)
                           (square (* 0.15 size) "solid" "white"))))
(define MINE (draw-mine CELL-SIZE))
(define CELL-MINE-BACKGROUND (overlay (square CELL-SIZE "outline" "black")
                                      (square CELL-SIZE "solid" "red")))
(define CELL-MINE (overlay MINE CELL-MINE-BACKGROUND ))
(define CELL-HIDDEN (overlay (square CELL-SIZE "outline" "dark green")
                             (square CELL-SIZE "solid" "lawn green")))
; draw-flag : Nat -> Image
; draws a flag of the given size
(define (draw-flag size)
  (overlay/align/offset "left" "bottom"
                        (overlay/align "left" "top"
                                       (line 0 (* size 0.8)
                                             (pen "black" (floor (* size 0.1))
                                                  "solid" "butt" "bevel"))
                                       (rotate 30
                                               (triangle (* size 0.4) "solid" "red")))
                        (* size 0.4 -0.866025)
                        0
                        (isosceles-triangle	(* size 0.4) 120 "solid" "black")))
(define CELL-FLAG (overlay (draw-flag CELL-SIZE) CELL-HIDDEN ))

(define CELL-NUMBER-COLOR
  (list "none" "blue" "dark green" "red" "purple" "maroon" "turquoise" "black" "grey"))

; message-background : Nat -> Image
; produces the background for the win/lose message scales with the size of the board in cells
(define (message-background size)
  (rectangle (* CELL-SIZE size) (* CELL-SIZE size 0.3) 200 "grey"))

; message-lose : String -> Image
; produces a lose text box with the given message
(define (message-lose message size)
  (overlay (beside (draw-mine (* 0.2 CELL-SIZE size))
                   (text/font message
                              (floor (* 0.06 CELL-SIZE size))
                              "black"
                              "default" "script"
                              "italic" "bold" #false))
           (message-background size)))

; message-win : String -> Image
; produces a win text box with the given message
(define (message-win message size)
  (overlay (beside (draw-flag (* 0.2 CELL-SIZE size))
                   (text/font message
                              (floor (* 0.06 CELL-SIZE size))
                              "black"
                              "default" "script"
                              "italic" "bold" #false))
           (message-background size)))

; draw-game : Game -> Image
; draws the game              
(define (draw-game g)
  (above/align "right" (draw-board  (game-board g))
               (text (if (game-rev? g)
                         "Reveal"
                         "Flag")
                     (floor (* CELL-SIZE 0.6))
                     "black" )))

; draw-board : Board -> Image
; draws the board
(define (draw-board b)
  (foldl (lambda (row rest) (above  rest row)) empty-image (map draw-row b)))

; draw-row : Row -> Image
; draws the row

(define (draw-row r)
  (foldr (lambda (cell rest) (beside cell rest)) empty-image (map draw-cell r)))
  

; draw-cell : Cell-> Image
; draws the cell 

(define (draw-cell ce)
  (cond [(hidden?  ce) CELL-HIDDEN]
        [(visible? ce) (draw-content (visible-con ce)) ]
        [(flagged? ce) CELL-FLAG]))


; draw-content : Content -> Image
; draws the content of a cell

(define (draw-content c)
  (cond [(and (number? c) (zero? c)) CELL-BACKGROUND]
        [(number? c) (overlay (text/font (number->string c)
                                         (round (* CELL-SIZE .6))
                                         (list-ref CELL-NUMBER-COLOR c)
                                         "default" "default"
                                         "italic" "bold" #false)
                              CELL-BACKGROUND)]
        [(boolean? c) CELL-MINE ]))

; draw-lose : Game -> Image
; draws the lose screen
(define (draw-lose game)
  (local [(define choice (random 10))]
    (overlay (message-lose (cond [(= choice 0) "Kaboom to You, Sir!"]
                                 [(= choice 1) "You've Been Atomized!"]
                                 [(= choice 2)
                                  "The Mine underwent a \nViolently Exothermic \nReaction"]
                                 [(= choice 3) "Not Raised Atop \nthe Montains by \nSome Monks Huh?"]
                                 [(= choice 4) "Didn't You Spend 27 Years \nin Ninja School?"]
                                 [(> choice 4) "You Lose!"])
                           (length (game-board game)))
             (draw-game game))))

; draw-win : Game -> Image
; draws the win screen
(define (draw-win game)
  (local [(define choice (random 10))]
    (overlay (message-win (cond [(= choice 0) "Congratulations!"]
                                [(= choice 1) "All Skill, No Luck"]
                                [(= choice 2) "Math is Cool, Right?"]
                                [(= choice 3) "On to the Next One!"]
                                [(= choice 4) "Those Years in Ninja \nSchool Really Payed Off"]
                                [(> choice 4) "You Win!"])
                          (length (game-board game)))
             (draw-game game))))

(define (draw-game-over game)
  (if (board-win? (game-board game))
      (draw-win game)
      (draw-lose game)))

;=== END ===






; === Excersise 4 ===

; process-mouse : Game Integer Integer MouseEvent -> Game
; handles the mouse events for the minesweeper game
(define (process-mouse game x y event)
  (if (and (mouse=? event "button-down") (on-board (game-board game) x y))
      (local [(define x-index (floor (/ x CELL-SIZE)))
              (define y-index (floor (/ y CELL-SIZE)))
              (define board (game-board game))
              (define cell (list-ref (list-ref board y-index) x-index))]
        (if (game-rev? game)
            (if (and (visible? cell) (all-adj-mines-flagged? x-index y-index board))
                (make-game (cell-flood x-index y-index board) #true)
                (make-game (reveal-cell x-index y-index board) #true))
            (make-game (toggle-flagged x-index y-index board) #false)))
      game))

;update-cell : Integer Integer [Cell -> Cell] Board -> Board
(define (update-cell x y updater board)
  (local [(define row (list-ref board y))
          (define cell (list-ref row x))]
    (list-update y (list-update x (updater cell) row) board)))

;list-update : Nat x [List-of X] -> [List-of X]
; replaces a element at the given index with the given x
(check-expect (list-update 0 0 (list 1 2 3)) (list 0 2 3))
(check-expect (list-update 1 1 (list 1 2 3)) (list 1 1 3))
(check-expect (list-update 2 2 '()) '())
(define (list-update index x list)
  (cond [(empty? list) '()]
        [(cons? list)  (if (zero? index)
                           (cons x (rest list))
                           (cons (first list) (list-update (sub1 index) x (rest list))))]))
  
        
; on-board? : Board Integer Integer -> Boolean
; determines whether the given coordinate is within the game board
(check-expect (on-board BOARD-2X2-BLANK -1 (* 1.5 CELL-SIZE)) #f)
(check-expect (on-board BOARD-2X2-BLANK (* 1.5 CELL-SIZE) (* 1.5 CELL-SIZE)) #t)
(check-expect (on-board BOARD-2X2-BLANK (* 2.5 CELL-SIZE) (* 1.5 CELL-SIZE)) #f)
(check-expect (on-board BOARD-2X2-BLANK (* 1.5 CELL-SIZE) (* 2.5 CELL-SIZE)) #f)

(define (on-board board x y)
  (local [(define height (* (length board) CELL-SIZE))
          (define width (* (length (first board)) CELL-SIZE))]
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

; reveal-cell : Nat Nat Board -> Board
; reveal the cell at the given X Y cords. floods fills surrounding non-mine cells if the cell is a 0.
(define (reveal-cell x y board)
  (local [(define cell (get-cell x y board))
          (define board-visible (update-cell x y cell-visible board))]
    (cond [(or (visible? cell) (flagged? cell)) board]
          [(boolean? (cell-content cell)) board-visible]
          [(zero? (cell-content cell)) (cell-flood x y board-visible)]
          [else board-visible])))

; cell-flood : Nat Nat Board -> Board
; floods fills surrounding non-mine cells.
(define (cell-flood x y board)
  (local [(define (cell-flood/update pos board-rest)
            (local [(define cell (get-cell (posn-x pos) (posn-y pos) board))]
              (if (boolean? (cell-content cell))
                  board-rest
                  (reveal-cell (posn-x pos) (posn-y pos) board-rest))))]
    (foldr cell-flood/update board (get-neighbor-indices x y (length board)))))

; toggle-flagged : Nat Nat Board -> Board
; toggles cell at the given location between flagged and hidden,
; does nothing to visible cells
(define (toggle-flagged x y board)
  (local [(define cell (get-cell x y board))]
    (cond [(visible? cell) board]
          [(hidden? cell) (update-cell x y cell-flagged board)]
          [(flagged? cell) (update-cell x y cell-hidden board)])))
                

; get-cell: Nat Nat Board -> Cell
; gets the cell at the specifies location
(define (get-cell x y board)
  (list-ref (list-ref board y) x))


; cell-content : Cell -> Content
; returns the contents of a cell
(check-expect (cell-content CELL-H0) 0)
(check-expect (cell-content CELL-V1) 1)
(check-expect (cell-content CELL-FM) #true)
(define (cell-content cell)
  (cond [(hidden? cell) (hidden-con cell)]
        [(visible? cell) (visible-con cell)]
        [(flagged? cell) (flagged-con cell)]))

; cell-visible : Cell -> Cell
; changes the cell state to visiable
(check-expect (cell-visible CELL-H0) CELL-V0)
(check-expect (cell-visible CELL-V1) CELL-V1)
(check-expect (cell-visible CELL-FM) CELL-VM)
(define (cell-visible cell)
  (make-visible (cell-content cell)))

; cell-flagged : Cell -> Cell
; changes the cell state to flagged
(check-expect (cell-flagged CELL-H0) CELL-F0)
(check-expect (cell-flagged CELL-V1) CELL-F1)
(check-expect (cell-flagged CELL-FM) CELL-FM)
(define (cell-flagged cell)
  (make-flagged (cell-content cell)))

; cell-hidden : Cell -> Cell
; changes the cell state to hidden
(check-expect (cell-hidden CELL-H0) CELL-H0)
(check-expect (cell-hidden CELL-V1) CELL-H1)
(check-expect (cell-hidden CELL-FM) CELL-HM)
(define (cell-hidden cell)
  (make-hidden (cell-content cell)))

; all-adj-mines-flagged? : Nat Nat Board -> Boolean
; checkes if all mine-cells surranding the given location is flagged
(check-expect (all-adj-mines-flagged? 1 1 (list (list CELL-H0 CELL-H0 CELL-H0)
                                                (list CELL-H0 CELL-H0 CELL-H0)
                                                (list CELL-H0 CELL-H0 CELL-H0))) #true)
(check-expect (all-adj-mines-flagged? 1 1 (list (list CELL-FM CELL-FM CELL-FM)
                                                (list CELL-H0 CELL-H0 CELL-H0)
                                                (list CELL-H0 CELL-FM CELL-H0))) #true)
(check-expect (all-adj-mines-flagged? 1 1 (list (list CELL-FM CELL-FM CELL-HM)
                                                (list CELL-H0 CELL-H0 CELL-H0)
                                                (list CELL-H0 CELL-FM CELL-H0))) #false)
(define (all-adj-mines-flagged? x y board)
  (local [; all-adj-mines-flagged?/check : Posn -> Boolean
          ; check for the cell at the given posn that the cell is a mine implies it is flagged
          (define (all-adj-mines-flagged?/check pos)
            (local [(define cell (get-cell (posn-x pos) (posn-y pos) board))]
              (or (number? (cell-content cell)) (flagged? cell))))]
    (andmap all-adj-mines-flagged?/check (get-neighbor-indices x y (length board)))))

; === Excersise 5 ===
;; generate-mine-board : Nat Nat -> Board
;; Generates a fixed board of the given size with the given number of mines
(check-error (generate-mine-board 10 101) "Cannot place 101 mines on a 10x10 game board.")
(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot place " (number->string mines)
                            " mines on a " (number->string size) "x" (number->string size)
                            " game board."))
      (local [(define empty-row (make-list size (make-hidden 0)))
              (define empty-board (make-list size empty-row))
              (define (add-mine dummy board)
                (local [(define x (random size))
                        (define y (random size))
                        (define cell (get-cell x y board))]
                  (if (mine? cell)
                      (add-mine 0 board)
                      (update-cell x y cell-mine board))))]
        (add-counts (foldr add-mine empty-board (make-list mines 0))))))

; mine? : Cell -> Boolean
; returns if the cell is a mine or not
(check-expect (mine? CELL-H0) #false)
(check-expect (mine? CELL-V1) #false)
(check-expect (mine? CELL-FM) #true)
(define (mine? cell)
  (boolean? (cell-content cell)))

; cell-mine : Cell -> Cell
; changes the content of the cell to a mine
(check-expect (cell-mine CELL-H0) CELL-HM)
(check-expect (cell-mine CELL-V1) CELL-VM)
(check-expect (cell-mine CELL-FM) CELL-FM)

(define (cell-mine cell)
  (cond [(visible? cell) (make-visible #t)]
        [(hidden? cell) (make-hidden #t)]
        [(flagged? cell) (make-flagged #t)]))

