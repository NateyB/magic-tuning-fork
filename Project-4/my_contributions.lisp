;;; Calculates the boolean entropy of a real number; as the function
(defun boolean_entropy(real)
    (if (< 0 real 1)
        (- (+ (* real (log2 real)) (* (- 1 real) (log2 (- 1 real)))))
        0))


;;; I wrote this function
(defun log2(real)
    (* 1.4426950408889634 (log real))) ;; 1/ln(2)*ln(real)

;;; I wrote this one
(defun remainder(examples attribute &aux (sum 0))
    (dolist (cur (get-attribute-values attribute) sum) ;;; Sum the attribute entropy stuff
        (incf sum (* (/ (length (collect-examples-with-this-value examples attribute cur)) (length examples))
                     (if (eq (length (collect-examples-with-this-value examples attribute cur)) 0)
                        0 ;;; This 0 is determined by the limit as these values approach 0
                        (boolean_entropy (/ (count-positives (collect-examples-with-this-value examples attribute cur)) (length (collect-examples-with-this-value examples attribute cur)))))))))


;;; I wrote this function as well: Calculate Quinlan's gain for each attribute
(defun max_gain (examples attributes &aux (max `(NIL -1)) (total_entropy (boolean_entropy (/ (count-positives examples) (length examples)))))
    (dolist (cur attributes (first max))
        (if (> (- total_entropy (remainder examples cur)) (second max))
            (setf max `(,cur ,(- total_entropy (remainder examples cur)))))))

;;; This function is written by me, with the exception of the "if (> count-...", which was provided.
(defun make-decision (example &optional (decision-tree *current-tree*) &aux (new_tree (second (first `(,(get-subtree example (rest decision-tree) (first decision-tree)))))))
  "Use this decision tree to classify this unclassified instance."
    (cond
        ((or (positive-example? new_tree) (negative-example? new_tree)) (get-classification new_tree))
        ((eq `? (get-classification new_tree)) (if (> (count-matching-leaves decision-tree '-)
           (count-matching-leaves decision-tree '+))
            (setf (car new_tree) '-)
              (setf (car new_tree) '+)))
        (t (make-decision example new_tree))))

;;; I wrote this function; gets a subtree from the tree based on the attribute's value in the example
(defun get-subtree(example tree root)
    (dolist (cur example)
        (if (eq (first cur) root)
            (return-from get-subtree (search-for-subtree tree (second cur))))))

;;; Gets the subtree from the tree with the appropriate value
(defun search-for-subtree(tree val)
    (dolist (cur tree)
        (if (eq (first cur) val)
            (return-from search-for-subtree cur))))

;;; I modified this function
(defun id3 (examples possible-attributes splitting-function)
  "A simple version of Quinlan's ID3 Program - see Machine Learning 1:1, 1986.
   Numeric feature values, noisy data and missing values are not handled."
 ;;; This function produces a decision tree that classifies the examples
 ;;; provided, using these attributes.  The splitting function determines
 ;;; which attribute should be used to split a collection of + and - examples.
 ;;; If all the examples are of the same type, a leaf node is returned.

 ;;; The resulting decision tree is a list of the form
 ;;;     (attribute (value1 subtree1) (value2 subtree2) ... )
 ;;;  or (decision  #-of-examples-in-training-set-located-here)
 ;;; In the first case, depending on the value of attribute, another decision
 ;;; tree must be traversed to make a decision.  In the second case, a
 ;;; decision is recorded, along with the number of examples from the
 ;;; training set that would be placed at this node.

 ;;  See RUN-ID3 for a nice user interface to this function.

 ;;  It is assumed that every example has a valid value for each attribute.
 ;;  The function VALIDATE-EXAMPLES can be used to pre-process examples.

     (cond ((null examples) '(? 0))  ; No more examples, an "undecided" node.
           ((all-positive? examples) `(+ , (length examples)))
           ((all-negative? examples) `(- , (length examples)))
           ((null possible-attributes) (error "Out of features - inconsistent data."))
               (t
                    ;;; I modified this function to contain this part:
                    (let* ((chosen (choose-attribute examples possible-attributes splitting-function))
                          (tree `(,chosen)))
                        (dolist (cur (get-attribute-values chosen) tree)
                            (setf tree (append tree `(,`(,cur ,(id3 (collect-examples-with-this-value examples chosen cur) (remove chosen possible-attributes) splitting-function))))))))))
