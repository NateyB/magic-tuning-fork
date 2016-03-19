;;; ************************************************************************************************
;;; This is my Constraint Satisfaction Problem via Sudoku solver!
;;; Created on 2015-10-24 by Nate Beckemeyer for his Artificial Intelligence class.
;;; This top list of functions are merely for the shell of solving sudoku puzzles.
;;; ************************************************************************************************


;;; Defining some global constants; here, the "basis" of Sudoku is the most fundamental unit and
;;; the square root of the dimension
(defconstant **BASIS** 3)
(defconstant **DIMENSION** (expt **BASIS** 2))
(defconstant **DOMAIN** (create-vector (1+ **DIMENSION**) 1))
(defvar **COUNT** 0)

;;; Creates a vector of natural numbers from start (0 by default) through end
(defun create-vector(end &optional (start 0))
 (if (< start end)
  (cons start (create-vector end (1+ start)))))

;;; Reads the input into a list
(defun parse-board(size)
 (if (> size 0)
  (cons (read) (parse-board (1- size)))))

;;; Displays the board in a human-friendly way
(defun display-board (board &aux listThing (count -1) (row `()))
 (dotimes (i **DIMENSION** board)
  (setf row `())
  (print (dotimes (j **DIMENSION** (reverse row))
   (setf row (cons (aref board (+ (* **DIMENSION** i) j)) row))))))

;;; Converts the board to a 1D array, for faster access
(defun to-array(origList &aux (count -1) (newArray (make-array (list (length origList)))))
 (dolist (cur origList newArray)
  (setf (aref newArray (incf count)) cur)))

;;; Does the board fully satisfy the constraints? Returns if this board is a solution.
(defun satisfies-constraints(board forward-checking? domains)
 (if board
  (dotimes (i (length board) t)
   (if (inconsistent-board-at-location board i forward-checking? domains)
    (return-from satisfies-constraints `NIL)))))

;;; Attempts to set location in board to val; if it cannot, return nil; else, return the new board
(defun attempt-move (board location val forward-checking? domains)
 (if (inconsistent-board-at-location board (and (setf (aref board location) val) location) forward-checking? domains)
  `NIL
  board))

;;; Determines whether the board is inconsistent at a certain location (or violates its domain)
;;; NOTE: Theoretically, only domain-consistent values should ever be passed, so I may be able to
;;; remove this check entirely. Will look into.
(defun inconsistent-board-at-location(board location forward-checking? domains)
 (if (aref domains location)
  (does-fail board location forward-checking? domains)))

;;; Determines if the value at location in the board is inconsistent with the constraints. That is,
;;; it checks to see if the value is allowed with the row, column, and subsquare.
(defun does-fail(board location forward-checking? domains &aux (count 0))
 (dolist (cur (get-neighbors location) `NIL)
  (if (and forward-checking? (not (eq location cur)) (eq (length (aref domains cur)) 1) (eq (first (aref domains cur)) (aref board location)))
   (return-from does-fail t)
   (if (eq (aref board location) (aref board cur))
    (if (> (incf count) 3)
     (return-from does-fail t))))))

;;; Gets the column indices from a location in the board
(defun column-from-location (location &optional (i 0))
 (if (< i **DIMENSION**)
  (cons (+ (mod location **DIMENSION**) (* i **DIMENSION**)) (column-from-location location (1+ i)))))

;;; Gets the row indices from a location in the board
(defun row-from-location (location &optional (i 0))
 (if (< i **DIMENSION**)
  (cons (+ (* **DIMENSION** (floor (/ location **DIMENSION**))) i) (row-from-location location (1+ i)))))

;;; Gets the subsquare indices from a location in the board
(defun square-from-location(location &aux (start (get-top-left location)) (subsquare `()))
 (dotimes (i **BASIS** subsquare)
  (dotimes (j **BASIS**)
   (push (+ (* i **DIMENSION**) j start) subsquare))))

;;; Gets the top-left corner of a subsquare, given a location in the board
(defun get-top-left(location)
 (+ (* (floor (/ (- location (* (floor (/ location **DIMENSION**)) **DIMENSION**)) **BASIS**)) **BASIS**) (* (floor (/ location (* **BASIS** **DIMENSION**))) (* **BASIS** **DIMENSION**))))

;;; Finds the next o in the board; the next variable that can take a value
(defun find-next-o(board spot)
 (dotimes (cur (- (expt **DIMENSION** 2) spot) `NIL)
  (if (eq (aref board (+ cur spot)) 0)
   (return-from find-next-o (+ cur spot)))))

;;; Creates all of the connected nodes in the grid
(defun get-connected-nodes(&aux connections)
 (dotimes (i **DIMENSION** connections)
  (setf connections (cons (column-from-location i) connections))
  (setf connections (cons (row-from-location (* i **DIMENSION**)) connections))
  (setf connections (cons (get-nth-subsquare i) connections))))

;;; Gets all of the neighbors of a point i
(defun get-neighbors(i)
 (append (column-from-location i) (row-from-location i) (square-from-location i)))

;;; Get's the nth subsquare in the grid, right to left, top to bottom
(defun get-nth-subsquare(n)
 (square-from-location (+ (* (floor (/ n **BASIS**)) **BASIS** **DIMENSION**) (* (mod n **BASIS**) **BASIS**))))

;;; Obtains all of the arcs in the grid
(defun obtain-arcs(&aux (arcs `()))
 (dolist (items (get-connected-nodes) arcs)
  (setf arcs (union (append (apply-arcs items) arcs) arcs))))

;;; Creates all combinations of 2 arcs from a list
(defun apply-arcs(items &aux arcs)
 (if (> (length items) 1)
  (append (dotimes (cur (1- (length items)) arcs)
   (setf arcs (cons `(,(first items) ,(nth (1+ cur) items)) arcs))) (apply-arcs (rest items)))))

;;; Generates the domains for a given spot in the board; if the spot already has a value, then its
;;; domain is that value only (so as not to overwrite the user-supplied variables)
(defun generate-domains(board &optional (spot 0))
 (if (< spot (length board))
  (if (eq (aref board spot) 0)
   (cons **DOMAIN** (generate-domains board (1+ spot)))
   (cons (list (aref board spot)) (generate-domains board (1+ spot))))))

;;; Given an arc and domains, renders the arc consistent
(defun handle-arc(arc domains &aux (revised `NIL) allowed)
 (dolist (x (aref domains (first arc)))
  (setf allowed `NIL)
  (dolist (y (aref domains (second arc)))
   (if (not (eq x y))
    (setf allowed t)))
  (if (not allowed)
   (and (setf (aref domains (first arc)) (remove x (aref domains (first arc)))) (setf revised t))))
 `(,revised ,domains))

;;; Removes a value from the neighbors of locations. Returns the new domains and the changelog.
;;; This function enables forward-checking.
(defun remove-from-neighbors-domains(domains location value &aux changelog)
 (dolist (i (get-neighbors location) (list domains changelog))
  (if (not (eq i location))
   (setf (aref domains i)
    (if (find value (aref domains i))
     (and (setf changelog (cons i changelog)) (remove value (aref domains i)))
     (remove value (aref domains i)))))))

;;; Given a list of locations in domains and a value, inserts value into domains. This is useful for
;;; restoring the domains to their value before a failed move. I use it to return domains to prior
;;; versions in forward-checking.
(defun revert-changes(domains changelog value)
 (dolist (i changelog domains)
  (setf (aref domains i) (union `(,value) (aref domains i)))))

;;; Runs the actual algorithms & gathers statistical information
(defun begin-playing()
 (dotimes (iteration (read) t)
  (let ((board (parse-board (expt **DIMENSION** 2))) (time (get-internal-real-time)))
   (format t "~%Board:")
   (display-board (to-array board))
   (format t "~%")
   (setf **COUNT** 0)
   (format t "~%Backtracking:")
   (format t "~%Solution:")
   (display-board (depth-first-backtracking (to-array board) (to-array (generate-domains (to-array board)))))
   (format t "~%")
   (format t "Time taken: ~d~%" (/ (- (- time (setf time (get-internal-real-time)))) 1000000.0))
   (format t "Number of backtracks: ~d~%~%" **COUNT**)

   (setf **COUNT** 0)
   (format t "Backtracking with forward checking:~%Domains:~%~A~%" (generate-domains (to-array board)))
   (format t "Solution:")
   (display-board (depth-first-backtracking (to-array board) (to-array (generate-domains (to-array board))) t))
   (format t "~%")
   (format t "Time taken: ~d~%" (/ (- (- time (setf time (get-internal-real-time)))) 1000000.0))
   (format t "Number of backtracks: ~d~%~%" **COUNT**)

   (setf **COUNT** 0)
   (display-board (ac3 (to-array board)))
   (format t "~%")
   (format t "Time taken: ~d~%" (/ (- (- time (setf time (get-internal-real-time)))) 1000000.0))
   (format t "Number of backtracks: ~d~%~%" **COUNT**))))
;;; ************************************************************************************************
;;; The functions below this line are implementations of solving algorithms
;;; ************************************************************************************************

;;; Runs depth-first-backtracking, starting at spot (0 by default) in the board
(defun depth-first-backtracking (board domains &optional forward-checking? (spot 0) &aux curLocation changelog)
 (if (not (setf curLocation (find-next-o board spot)))
  (return-from depth-first-backtracking board))
 (dolist (curVal (aref domains curLocation) board)
  (let ((result (attempt-move board curLocation curVal forward-checking? domains)))
   (if (and result forward-checking?)
    (and (setf info (remove-from-neighbors-domains domains curLocation curVal)) (setf domains (first info)) (setf changelog (second info))))
   (if (and result (incf **COUNT**))
    (if (satisfies-constraints (depth-first-backtracking (setf board result) domains forward-checking? (1+ spot)) forward-checking? domains)
     (return-from depth-first-backtracking board)))
   (if forward-checking?
    (setf domains (revert-changes domains changelog curVal)))))
 (return-from depth-first-backtracking (and (setf (aref board curLocation) 0) `NIL)))

;;; Implementation of Arc-Consistency 3
(defun ac3(board &optional forward-checking? &aux arc-queues domains arc)
 (setf domains (to-array (generate-domains board)))
 (setf arc-queues (obtain-arcs))
 (loop while (> (length arc-queues) 0) do
  (setf arc (pop arc-queues))
  (setf domains (handle-arc arc domains))
  (if (first domains)
   (if (eq (length (aref (second domains) (first arc))) 0)
    (return-from ac3 `NIL)
    (dolist (cur (get-neighbors (first arc)))
     (if (not (or (eq cur (second arc)) (eq cur (first arc))))
      (push `(,cur ,(first arc)) arc-queues)))))
  (setf domains (second domains)))
 (format t "Backtracking after AC3 Preprocessing:~%Domains:~%~A~%" domains)
 (format t "Solution:")
 (depth-first-backtracking board domains forward-checking?))