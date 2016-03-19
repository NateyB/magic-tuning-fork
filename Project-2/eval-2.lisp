;;;********************************************************************************************
;;;*                            *
;;;*            Board Evaluation Routines         *
;;;*                            *
;;;********************************************************************************************
;;;
;;; WINNING is a very high value returned when a winning configuration is detected.
;;; LOSING is a very low value returned when a losing configuration is detected.
;;;
(defvar *WINNING* (* 10 *WIN*))
(defvar *LOSING*  (* 10 *LOSE*))

;;*********************************************************************************************
;; This is the board-evaluator function called from minimax to evaluate a board. It returns an
;; integral value that reflect the promise of the input board.
;; INPUT: Current board
;;*********************************************************************************************
(defun static_eval (board)
 (rank_board (convert_to_array board)))

;;*********************************************************************************************
;; The following function convert an input board to array representation. It
;; is this array representation that is passed by the function static_eval() to
;; the function rank_board()
;; INPUT: current board.
;;*********************************************************************************************
(defun convert_to_array (board)
  (let ((index 0)
  (squares (make-array (* *Dimension* *Dimension*))))
    (dotimes (i *Dimension* squares)
       (dotimes (j *Dimension*)
          (setf (aref squares index)
          (aref board i j))
          (incf index)))))

;;*********************************************************************************************
;; This function, on being given the array representation of a board returns an integer that is
;; the rank of the board from the view-point of the maximising player. Note that the variable
;; win_positions is a list of all possible winning configurations.
;; INPUT: array representation of current board.
;;
;; My evaluation function is rather simple: only blank spaces need
;; to be evaluated, with the exception of initial checks for a win condition.
;;
;; Here's how it works:
;; 1) A win condition returns *WINNING*.
;; 2) A lose condition returns *LOSING*.
;; 3) For each unique square resulting in a connect 4,
;;  a) 7 points if there are 3 black pieces in its rays of 3 neighboring pieces
;;    -7 points if there are 3 white pieces in its rays of 3 neighboring pieces
;;  b) 3 points if there are 2 black pieces in its rays of 3 neighboring pieces
;;    -3 points if there are 2 white pieces in its rays of 3 neighboring pieces
;;  c) 1 point  if there  is 1 black piece  in its rays of 3 neighboring pieces
;;    -1 point  if there  is 1 white piece  in its rays of 3 neighboring pieces
;;*********************************************************************************************
(defun rank_board (board_array &aux (sum-total 0))
 (dolist (current *win_positions* sum-total)
  (let ((amount (assign-points board_array current)))
   (setf sum-total (+ sum-total amount)))))

(defun assign-points(board win-sequence &aux (players `(,*Human-Team* ,*Computer-Team*)))
 (let ((subset (select-subset board win-sequence)))
    (if (set-xor subset players)
     (get-points subset)
     0)))

(defun team-multiplier(team)
 (if (eq team *Human-Team*)
  -1
  1))

(defun get-points(position)
 (setf position (set-difference position `(,*Empty-Slot*)))
 (* (team-multiplier (first position)) (point-lookup (length position))))

(defun point-lookup(sequence-length)
 (nth sequence-length `(0 5 6 7 ,*WINNING*)))
 ; 0 <- 0; 1 <- 1; 2 <- 3; 3 <- Winning/(Dimension^2); 4 <- WINNING

(defun set-xor(a b &aux (value 'NIL))
 (if (find (pop b) a)
  (setf value (not value)))
 (if (find (pop b) a)
  (not value)
  value))

(defun select-subset(board sequence &aux (subset `()))
 (dotimes (index (length sequence))
  (setf subset (append subset (list (aref board (pop sequence))))))
 subset)