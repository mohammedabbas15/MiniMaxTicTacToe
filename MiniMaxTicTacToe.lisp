; Write a LISP program to play Tic-Tac-Toe on a size 4x4 game board.
; Your program must use the minimax-decision search algorithm and should be
; invoked by the function call:
; Remember, this is a purely deterministic environment.
; There is no need to do any kind of random number generation.
; You'll need to represent the each space in one of three ways:
; Player Max, Player Min, and an empty space.
 
(write-line "######################")
(write-line "Welcome to Tic Tac Toe")
(write-line "######################")

; represents the GameBoard as strings from 1 to 16
; 4 squares on each straight and 
; 2 diagonals that will make the board (4 x 4)
(setf GameBoard (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16"))

; representing a 4x4 game board
(defun printBoard ()
    (format t "----------------------")
    (terpri)
    (format t "| ")
    (format t (nth 0 GameBoard))
    (format t " | ")
    (format t (nth 1 GameBoard))
    (format t " | ")
    (format t (nth 2 GameBoard))
    (format t " | ")
    (format t (nth 3 GameBoard))
    (format t " |")
    (terpri)
    (format t "----------------------")
    (terpri)
    (format t "| ")
    (format t (nth 4 GameBoard))
    (format t " | ")
    (format t (nth 5 GameBoard))
    (format t " | ")
    (format t (nth 6 GameBoard))
    (format t " | ")
    (format t (nth 7 GameBoard))
    (format t " |")
    (terpri)
    (format t "----------------------")
    (terpri)
    (format t "| ")
    (format t (nth 8 GameBoard))
    (format t " | ")
    (format t (nth 9 GameBoard))
    (format t " | ")
    (format t (nth 10 GameBoard))
    (format t " | ")
    (format t (nth 11 GameBoard))
    (format t " |")
    (terpri)
    (format t "----------------------")
    (terpri)
    (format t "| ")
    (format t (nth 12 GameBoard))
    (format t " | ")
    (format t (nth 13 GameBoard))
    (format t " | ")
    (format t (nth 14 GameBoard))
    (format t " | ")
    (format t (nth 15 GameBoard))
    (format t " |")
    (terpri)
    (format t "----------------------")

)

(defun humanMove(select)
     (setf value (- (parse-integer select) 1))

     (if (or (string= (nth value GameBoard) "X")(string= (nth value GameBoard) "O"))
          (print "Choose another square!")
          (setf (nth value GameBoard) "X")
     )   
     (terpri)
)

(defun makeBoard(player position board)
    (if (= position 0)
        (cons player (cdr board))
        (cons (car board) (makeBoard player (- position 1) (cdr board)))
    )
)

(defun moveValue(board isMaximizingPlayer)
    (setf win (checkForWin board))
    ;(setf win (didyouwin board))
    (if win
        (if (string= win "X")
            1
            3
        )
        (if (not (availableSpots board))
            2
            (if isMaximizingPlayer
              (maxValue board)
              (minValue board)
            )
        )
    )

)

(defun maxValue(board)
    (setf max-value 0)
    (loop for position in (availableSpots board)
        do
        (setf value (moveValue (makeBoard "O" position board) nil))
        (if (> value max-value)
            (setf max-value value)
        )
        (if (= max-value 3)
            (return)
        )
    )
    max-value
)

(defun minValue(board)
    (setf min-value 4)
    (loop for position in (availableSpots board)
        do
        (setf value (moveValue (makeBoard "X" position board) t))
        (if (< value min-value)
            (setf min-value value)
        )
        (if (= min-value 1) 
            (return)
        )
    )
    min-value
)

(defun minimaxDecision()
    (setf bestPosition 0)
    (setf bestValue 0)
    (loop for position in (availableSpots GameBoard)
        do
        (print "Working on spot number: ")(print position)
        (setf value (moveValue (makeBoard "O" position GameBoard) nil))
        (if (> value bestValue)
            (progn 
              (setf bestValue value)
              (setf bestPosition position)
            )
        )
        (if (= bestValue 3)
            (progn
                (setf (nth bestPosition GameBoard) "O")
                (return)
            )
        )
    )
    (setf (nth bestPosition GameBoard) "O")
)

(defun availableSpots(board)
    (setf spots (list))
    (loop for x in (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        do
            (if (not(or(string= (nth x board) "X") (string= (nth x board) "O")))
                (setf spots (cons x spots))
            )
    )
    spots
)

(defun checkForWin(board)
    (cond ((and(string= (nth 0 board) "X") (string= (nth 1 board) "X") (string= (nth 2 board) "X") (string= (nth 3 board) "X")) "X")
          ((and(string= (nth 4 board) "X") (string= (nth 5 board) "X") (string= (nth 6 board) "X") (string= (nth 7 board) "X")) "X")
          ((and(string= (nth 8 board) "X") (string= (nth 9 board) "X") (string= (nth 10 board) "X") (string= (nth 11 board) "X")) "X")
          ((and(string= (nth 12 board) "X") (string= (nth 13 board) "X") (string= (nth 14 board) "X") (string= (nth 15 board) "X")) "X")
          ((and(string= (nth 0 board) "X") (string= (nth 4 board) "X") (string= (nth 8 board) "X") (string= (nth 12 board) "X")) "X")
          ((and(string= (nth 1 board) "X") (string= (nth 5 board) "X") (string= (nth 9 board) "X") (string= (nth 13 board) "X")) "X")
          ((and(string= (nth 2 board) "X") (string= (nth 6 board) "X") (string= (nth 10 board) "X") (string= (nth 14 board) "X")) "X")
          ((and(string= (nth 3 board) "X") (string= (nth 7 board) "X") (string= (nth 11 board) "X") (string= (nth 15 board) "X")) "X")
          ((and(string= (nth 0 board) "X") (string= (nth 5 board) "X") (string= (nth 10 board) "X") (string= (nth 15 board) "X")) "X")
          ((and(string= (nth 3 board) "X") (string= (nth 6 board) "X") (string= (nth 9 board) "X") (string= (nth 12 board) "X")) "X")
          ((and(string= (nth 0 board) "O") (string= (nth 1 board) "O") (string= (nth 2 board) "O") (string= (nth 3 board) "O")) "O")
          ((and(string= (nth 4 board) "O") (string= (nth 5 board) "O") (string= (nth 6 board) "O") (string= (nth 7 board) "O")) "O")
          ((and(string= (nth 8 board) "O") (string= (nth 9 board) "O") (string= (nth 10 board) "O") (string= (nth 11 board) "O")) "O")
          ((and(string= (nth 12 board) "O") (string= (nth 13 board) "O") (string= (nth 14 board) "O") (string= (nth 15 board) "O")) "O")
          ((and(string= (nth 0 board) "O") (string= (nth 4 board) "O") (string= (nth 8 board) "O") (string= (nth 12 board) "O")) "O")
          ((and(string= (nth 1 board) "O") (string= (nth 5 board) "O") (string= (nth 9 board) "O") (string= (nth 13 board) "O")) "O")
          ((and(string= (nth 2 board) "O") (string= (nth 6 board) "O") (string= (nth 10 board) "O") (string= (nth 14 board) "O")) "O")
          ((and(string= (nth 3 board) "O") (string= (nth 7 board) "O") (string= (nth 11 board) "O") (string= (nth 15 board) "O")) "O")
          ((and(string= (nth 0 board) "O") (string= (nth 5 board) "O") (string= (nth 10 board) "O") (string= (nth 15 board) "O")) "O")
          ((and(string= (nth 3 board) "O") (string= (nth 6 board) "O") (string= (nth 9 board) "O") (string= (nth 12 board) "O")) "O")
    )
)

(defun startGame()
  (loop
     (printBoard)
     (terpri)
     (write-line "#### Choose a square number, or enter 'q' to quit ####")
     (terpri)
     (setq selection (read-line))

     (if (string= selection "q")
          (return)
     )

     (humanMove selection)
     (minimaxDecision)

    (if (checkForWin GameBoard)
        (return)
    )

     (terpri)
  )
)

(startGame)



