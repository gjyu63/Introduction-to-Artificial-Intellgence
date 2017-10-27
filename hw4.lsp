;
; CS161 Winter 2017: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  (cond 
      ((< n 1) nil)
      ((< k 1) nil)
      ((< k c) nil) ; base case
      (t (+ (* (- n 1) k) c))
    )
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
  (cond 
      ((< n 1) nil)
      ((< k 1) nil)
      ((< k c) nil) ; base case
      (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))
; Helper function of at-most-one-color
; Takes in an positive integer and returns its negation


; Helper function of at-most-one-color
; at-most-one-color-helper takes a node number n, a color 
; number c and max color index k, and assigned color a
; It returns clauses (-index of variable for n a k and -index
; of variable for n a k) for every b comes after a
(defun at-most-one-color (n c k)
  (cond 
      ((< n 1) nil)
      ((< k 1) nil)
      ((< k c) nil) ; base case
      (t (append (amoc-helper n c k c) (at-most-one-color n (+ c 1) k)))))

; amoc-helper is the help function of the at-most-one-color
; it negates all the values corresponding to the color except the color c
;
(defun amoc-helper(n c k i)
  (cond 
    ((< n 1) nil)
    ((< k 1) nil)
    ((< k c) nil) ; base case
    ((= i c) (amoc-helper n (+ c 1) k i))
    (t (cons (list (- 0 (node2var n i k)) (- 0 (node2var n c k))) (amoc-helper n (+ c 1) k i)))
    )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
(defun generate-node-clauses (n k)
    (cond 
      ((< n 1) nil)
      ((< k 1) nil) ; base case
      (t (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))))

; Helper function of generate-edge-clauses
; generate-edge-clauses-helper takes in two nodes n1 and n2,
; a color index c, and a maximum color index k
; It returns (- index of variable n1 c k  - index of variable n2 c k)
; for every color index c and those come after c
(defun generate-edge-clauses-helper (node1 node2 c k)
  (cond 
    ((< node1 1) nil)
    ((< node1 1) nil)
    ((< c 1) nil)
    ((< k c) nil)
    (t (cons (list (- 0 (node2var node1 c k)) (- 0 (node2var node2 c k)))
    (generate-edge-clauses-helper node1 node2 (+ c 1) k)) 
    )
  )
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
  (let* ( (x (car e))
    (y (car(cdr e))))
    (cond 
      ((< k 1) nil)
      ((< x 1) nil)
      ((< y 1) nil)
      (t (generate-edge-clauses-helper x y 1 k))
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
