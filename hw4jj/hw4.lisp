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
  (load "~/desktop/161hw1/hw4/hw4thought.lisp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;

; 这个function就是用那个公式 
; 因为我们要做的是把node n 转换成一个 variable index
; use the provided equation: variable index = (n-1)*k + c
; 其实这里做的事情的意义就是把 node n gets color c 转换成 表达式 ，你会怎么做呢？
; note : be careful of the corner case (base case)
(defun node2var (n c k)
	(cond 	((or (= n 0) (= c 0) (= k 0)) nil)
			((> c k) nil)
			(t (+ (* (- n 1) k) c))

		)
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."

; 这个function就是说 node n 肯定是从set {c,c+1,...,k} 选至少一个颜色
; 其实这里做的事情的意义就是把"node n gets at least one color from the set {c,c+1,...,k}."转换成 表达式 
; 为了方便理解 举例子说 如果选了c 也还可以选c + 1 一直到k为止， 所有相对应的variable index 我们全部存在一个list里
; note : 这里 get this color 你要和上面func1联系起来
(defun at-least-one-color (n c k)
	(cond 	((or (= n 0) (= c 0) (= k 0)) nil)
			((> c k) nil)
			(t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k )))
			;(t (list (node2var n c k) (at-least-one-color n (+ c 1) k )))

		)
  )

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
; 其实这里做的事情的意义就是把"node n gets at most one color from the set {c,c+1,...,k}."转换成 表达式 
; 想象如果你选了1 那么 2 3 4 ...k就一定是不选的 需要negate 它(reminder: negate means negative)
; note: at most one, 需要考虑全部颜色都没有的时候 就是所有都是negate的时候
;

(defun negate-list(l)
		
	(cond 	((null l) nil)
    		(t (append (list (- (car l))) (negate-list (cdr l))))
    )

)

(defun at-most-one-color (n c k)
	(let
      (
      (l  (at-least-one-color n 1 k ))
      );end declaration
    (cond
    	((> (- c 2) K) nil)
        ((= (- c 1)  k) (list (negate-list l) ) )
        (t (append (list (append (negate-list (butlast l (- k (- c 1))))  (list (nth (- c 1) l)) (negate-list (nthcdr c l))) ) (at-most-one-color n (+ c 1) k))  )
       ; (t  (append (append (negate-list (butlast l (- k (- c 1))))  (list (nth (- c 1) l)) (negate-list (nthcdr c l)) ) (at-most-one-color n (+ c 1) k)) ) 
    );end cond
  );end let	

)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
; 其实想一下  (at most one) and (at least one ) = 1
(defun generate-node-clauses (n k)
	;(append   (at-least-one-color n 1 k ) (at-most-one-color n 1 k))
	(append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k)) 
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
; 其实这里做的事情的意义就是把"the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."转换成 表达式 
; cannot have the same color 意味着什么呢？
; 大概就是 结合一下第一个function 如果have the same color 那就会出现第一个 function的variable index 
; note 这里千万不能说color一样的哦
;

;(defun generate-edge-clauses (e k)
 ; )

(defun generate-edge-clauses (e k)
	(generate-edge-clauses-helper e 1 k)
)

(defun generate-edge-clauses-helper (e c k)
	(cond
		((> c k) NIL)
		(t
			(append (list (list (- (node2var (first e) c k)) (- (node2var (second e) c k))))
					(generate-edge-clauses-helper e (+ 1 c) k))
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
