;;;*******************************************************************************************
;;;*                                                 *
;;;*            Minimax and alpha-beta procedures (integrated)                       *
;;;*                                                 *
;;;*******************************************************************************************
;;;
;;;*******************************************************************************************
;;;  Top level decision procedure to implement minimax procedure.  If the cutoff
;;; depth is reached, it evaluates the board with static_eval and the number of nodes
;;; evaluated in the current search process is updated.  If the cutoff
;;; depth is not reached, find out if the current board contains a winning or losing
;;; configuration.  If so, return the winning or losing value and the incremented
;;; boards-evaluated.  If not, evaluate the current board by calling either max-value or
;;; min-value depending on whether it is my turn to make a move. Returns a list of three numbers:
;;; (i) the number of boards evaluated,
;;; (ii) the slot corresponding to the successor with the best evaluation, and
;;; (iii) that value.
;;;
;;; function ALPHA-BETA-SEARCH(state) returns an action v ←MAX-VALUE(state,−∞,+∞)
;;;  return the action in ACTIONS(state) with value v
;;;
;;;*******************************************************************************************

(defun minimax-decision (board level alpha-beta?
                   &optional (my-turn t) (alpha *MINUS-INFINITY*) (beta *PLUS-INFINITY*))
    (max-value board 1 level alpha-beta? alpha beta)
)



;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are
;;; evaluated by calling the minimax procedure after decrementing the level and changing the
;;; player.  This function then returns the maximum evaluation of the successor nodes.
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, beta is returned, otherwise
;;; alpha is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;;
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;        boards-evaluated - number of boards evaluated so far
;;;
;;; function MAX-VALUE(state, α, β) returns a utility value
;;;  if TERMINAL-TEST(state) then return UTILITY(state) v ← −∞
;;;  for each a in ACTIONS(state) do
;;;   v ← MAX(v, MIN-VALUE(RESULT(s,a), α, β))
;;;   if v ≥ β then
;;;    return v
;;;   α←MAX(α, v)
;;;  return v
;;;*******************************************************************************************

(defun max-value (board player level alpha-beta? alpha beta &aux children (val `(0 -1 ,*MINUS-INFINITY*)))
 (if (or (eq level 0) (is-terminal board))
  (return-from max-value `(1 -1 ,(static_eval board))))
 (setf children (successors board (to-color player)))
 (setf children (remove-nil children))
 (dolist (child children val)
  (let ((result (min-value (first child) (- player) (1- level) alpha-beta? alpha beta)))
   (if (> (third result) (third val))
    (setf val `(,(+ (first result) (first val)) ,(second child) ,(third result)))
    (setf val `(,(+ (first result) (first val)) ,(second val) ,(third val))))
   (if (and alpha-beta? (>= (third val) beta))
    (return-from max-value val))
   (setf alpha (max alpha (third val))))))

;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are
;;; evaluated by calling the minimax procedure after decrementing the level and changing the
;;; player.  This function then returns the minimum evaluation of the successor nodes.
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, alpha is returned, otherwise
;;; beta is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;;
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;
;;; function MIN-VALUE(state, α, β) returns a utility value
;;;  if TERMINAL-TEST(state) then return UTILITY(state) v ← +∞
;;;  for each a in ACTIONS(state) do
;;;   v ← MIN(v, MAX-VALUE(RESULT(s,a) , α, β))
;;;   if v ≤ α then
;;;    return v
;;;   β ← MIN(β, v)
;;;  return v
;;;*******************************************************************************************

(defun min-value (board player level alpha-beta? alpha beta &aux (val `(0 -1 ,*PLUS-INFINITY*)))
 (if (or (eq level 0) (is-terminal board))
  (return-from min-value `(1 -1 ,(static_eval board))))
 (setf children (successors board (to-color player)))
 (setf children (remove-nil children))
 (dolist (child children val)
  (let ((result (max-value (first child) (- player) (1- level) alpha-beta? alpha beta)))
   (if (< (third result) (third val))
    (setf val `(,(+ (first result) (first val)) ,(second child) ,(third result)))
    (setf val `(,(+ (first result) (first val)) ,(second val) ,(third val))))
   (if (and alpha-beta? (<= (third val) alpha))
    (return-from min-value val))
   (setf beta (min beta (third val))))))


;;; Utility methods

;; Boolean: Is the board a game end (full or a win condition?)
(defun is-terminal (board)
 (if (full? board)
  t
  (is-win-condition board)))

;; Color: Converts the player value to a color
(defun to-color(player)
 (if (eq player 1)
  *COMPUTER-TEAM*
  *HUMAN-TEAM*))

;; List: Removes nil children from the list of successors
(defun remove-nil(successors &aux existers)
 (dolist (current successors existers)
  (if (not (eq (first current) `NIL))
   (setf existers (cons current existers)))))

;; Boolean: Determines if the board is a win condition
(defun is-win-condition (board)
 (let ((amount (static_eval board)))
  (if (>= (abs amount) *WIN*)
   amount
   `NIL)))