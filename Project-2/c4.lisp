;;;  This is a program which plays a game of Connect Four with a human.
;;;  To stop the game at any point, press CTRL-C CTRL-C.
;;;*******************************************************************************************

;;;  Global constants initialised here
(defconstant *Dimension* 7)
(defconstant *WIN* 100000)
(defconstant *LOSE* -100000)
(defconstant *PLUS-INFINITY* (* 100 *WIN*))
(defconstant *MINUS-INFINITY* (* 100 *LOSE*))
(defconstant *Computer-Team* 'black)
(defconstant *Human-Team* 'white)
(defconstant *Empty-Slot* 'empty)

;; Contains the history of all moves in the game (slots).
(defvar move-history `())

;;##############################################################################################
;;  This is the function to execute from top-level.  It
;;  prints the title and calls the Play function.
;;  OPTIONAL INPUTS:  Prune?  T or NIL to turn alpha-beta pruning on (default) or off.
;;                    Depth  A number to indicate maximum tree expansion.  (Default = 3).
;;##############################################################################################
(defun Connect4 ()
  (format t "Connect Four ~% ~%")
  (format t "Do you want to play first ? (y/n) ~%")
  (let ((My-Turn? (eq 'n (read)))
	(Root (New-Board)))
     (format t "Do you want alpha-beta ? (y/n) ~%")
     (let ((Prune? (eq 'y (read))))
       (format t "Enter depth of search (1 - 4) : ")
       (let ((Depth (read)))
	 (Play Prune? Depth My-Turn? Root)))))

;;##############################################################################################
;;  This function alternates calling Human-Move and Computer-Move.
;;  INPUTS:  Prune?   T or NIL for alpha-beta pruning.
;;           Depth
;;           My-Turn  T if Player 2 should move.  NIL if Player 1.
;;           Current-Board  The current board configuration.
;;##############################################################################################
(defun Play (Prune? Depth My-Turn Current-Board &aux value)
  (Print-Node Current-Board)
  (cond ((< (setq value (static_eval Current-Board))  *LOSE*)
           (format t "OK smart guy, you won!! (Don't hope to repeat it)~%~%")
           'BYE)
        ((> value *WIN*)
           (format t "OK, I won!! (You know I was designed that way)~%~%")
           'BYE)
        ((full? Current-Board)
;	 (every #'(lambda (l) (null (first l))) (successors Current-Board (get-color My-Turn)))
         (format t "The game ends in a tie!! Enjoyed playing with you.~%~%")
         'BYE)
        (t
	 (if My-Turn
	     (Play Prune? Depth (not My-Turn)
			      (time (Computer-Move Current-Board Prune? Depth)))
	   (Play Prune? Depth (not My-Turn) (Human-Move Current-Board))))))

;;##############################################################################################
;;  This function generates the color of the token to be dropped into a slot from the boolean
;; variable Turn.
;; INPUT: Turn
;;##############################################################################################
(defun get-color (Turn)
   (if Turn
       *Computer-Team*
     *Human-Team*))

;;##############################################################################################
;;  This function accepts and checks a move by the human competitor.  It returns
;;  the new board configuration after player moves.
;;  INPUT:  Current-Board
;;##############################################################################################
(defun Human-Move (Current-Board)
  (format t "~%Your turn,  ")
  (let ((Slot (Player-Chooses-Slot)))
  (let ((Next-Board (Drop-Token Current-Board Slot *Human-Team*)))
    (cond ((null Next-Board)
	   (format t "~%That slot is full.  Try again.")
	   (Human-Move Current-Board))
	  (t (if (>= Slot 0)
      (setf move-history (cons Slot move-history)))
     Next-Board)))))

;;##############################################################################################
;;  This function generates the move by the computer.
;;  Computer-Move analyzes the game tree whose root is Current-Board using MINIMAX searching
;;  (with the option of alpha-beta pruning).  It will return the new board configuration
;;  after the computer chooses the best move.
;;  INPUT:  Current-Board
;;##############################################################################################
(defun Computer-Move (Current-Board Prune Depth)
  (let ((decision (minimax-decision Current-Board Depth Prune)))
    (format t "~%~%~%Number of boards evaluated --> ~A~%~%" (first decision))
    (format t "Dropped token in slot ~A~%" (1+ (second decision)))
    (setf move-history (cons (second decision) move-history))
    (Drop-Token Current-Board (second decision) *Computer-Team*)))

;;##############################################################################################
;;  This function allows a player to choose a slot number that is between 1 and the board
;; dimension.  It returns the chosen slot number.
;;##############################################################################################
(defun Player-Chooses-Slot ()
  (format t "~&Choose slot (1..~d) (input a negative to undo, sacrificing your turn):  " *Dimension*)
  (let ((Players-Choice (read)))
    (cond ((and (numberp Players-Choice)
		(<= Players-Choice *Dimension*))
	   (1- Players-Choice))
	  (t
	   (format t "Invalid choice, please choose again~%")
	   (Player-Chooses-Slot)))))

;;##############################################################################################
;;  This function creates a brand new board
;;##############################################################################################
(defun New-Board ()
  (make-array (list *Dimension* *Dimension*) :initial-element *Empty-Slot*))

;;##############################################################################################
;;  This function returns the new board configuration that results when
;;  a token is dropped in a given slot of the current board configuration.
;;  It copies the current configuration into Next-Board, and then modifies
;;  Next-Board based on whose turn it is and how far the token can drop in
;;  the given slot.  If the slot is full, this function returns NIL.
;;  INPUTS:  Current-Board  Current board configuration.
;;           Slot           Number of slot in which token is to be dropped.
;;           color          color of the token to be dropped
;;           Bottom-Row     Number of row that the token is dropping through.
;;
;; I modified this function slightly to account for undoing move.
;;##############################################################################################
(defun Drop-Token (Current-Board Slot color)
 (if (< Slot 0)
  (undo-moves Current-Board (abs Slot))
  (if (not (eq (aref Current-Board 0 Slot) *Empty-Slot*))
      nil;; slot is full
    (let ((Next-Board (Copy-of-Board Current-Board)))
      (do ((Bottom-Row 0 (1+ Bottom-Row)))
	  ((or (= Bottom-Row (1- *Dimension*)) ; reached bottom of slot
	       (not (eq (aref Current-Board (1+ Bottom-Row) Slot) *Empty-Slot*)))
					;slot is filled from one row down
	   (setf (aref Next-Board Bottom-Row Slot) color)
	   Next-Board))))))

;;##############################################################################################
;; This function returns t if Current-Board is full, otherwise returns nil
;;##############################################################################################
(defun full? (Current-Board)
  (dotimes (i *Dimension* t)
	   (when (eq (aref Current-Board 0 i) *Empty-Slot*)
	     (return nil))))

;;##############################################################################################
;;  This function returns a copy of a board.  The copy can be modified
;;  without affecting the original.
;;##############################################################################################
(defun Copy-Of-Board (Board)
  (let ((newBoard (make-array (list *Dimension* *Dimension*))))
    (dotimes (i *Dimension* newBoard)
	     (dotimes (j *Dimension*)
		      (setf (aref newBoard i j) (aref Board i j))))))

;;##############################################################################################
;;  This following functions print a node (board) from the game tree.
;;##############################################################################################
(defun Print-Node (Node)
  (Line *Dimension*)
  (dotimes (i *Dimension*)
	  (dotimes (j *Dimension*)
		   (cond ((eq (aref Node i j) *Computer-Team*) (format t "|  *  |"))
			 ((eq (aref Node i j) *Human-Team*) (format t "|  O  |"))
			 (t (format t "|     |"))))
		    (format t "~%"))
  (Line *Dimension*))

(defun Line (Length)
 (format t "~&")
 (dotimes (i Length)
           (format t "___~A___" (1+ i)))
 (format t "~%"))

;;##############################################################################################
;;  This function generates all the successors of a given board. A single board is constructed
;; as a list of lists. A non-existent successor (suppose a slot is full) is represented by an
;; empty list.  Returns pairs of boards and the slot number in which the token is dropped to
;; to generate that board.
;; INPUT: Board -> current board
;;	  color -> the color of token which is to be dropped to produce successors.
;;##############################################################################################
(defun successors (Board color &optional (Slot 0))
  (if (= Slot  *Dimension*)
      nil
    (cons (list (Drop-Token Board Slot color) Slot) (successors Board color (1+ Slot)))))

;;**************************************************************************************************
;; I wrote all functions beneath this line.
;;**************************************************************************************************

;;**************************************************************************************************
;; This function undrops a single token from a slot; there is no error checking done inside of the
;; method, so please be careful when using; returns the new board.
;;**************************************************************************************************
(defun undrop-token(Current-Board Slot)
 (dotimes (i *Dimension* Current-Board)
  (if (and (not (eq (aref Current-Board i Slot) *Empty-Slot*)) (setf (aref Current-Board i Slot) *Empty-Slot*))
   (return Current-Board))))

;;**************************************************************************************************
;; Given a number of moves x, return a board that does not have the past x moves.
;;**************************************************************************************************
(defun undo-moves(Current-Board number-moves)
 (setf number-moves (min (1- number-moves) (length move-history)))
 (dotimes (i number-moves Current-Board)
  (setf Current-Board (undrop-token Current-Board (pop move-history)))))