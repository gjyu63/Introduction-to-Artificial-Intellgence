;
; CS161 Hw3: Sokoban (Feng Shi, UID 404-246-421)
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )
    
;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
; 
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE 1: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; helper function for detecting whether there exists a box not in goal
(defun no-box-on-row (r) 
  (cond 
    ((null r) T)
    ((= (car r) 2) nil)
    (T (no-box-on-row (cdr r))))
  )

(defun goal-test (s)
  (cond 
    ((null s) T)
    ((null (no-box-on-row (car s))) nil)
    (T (goal-test (cdr s)) )
  ));end defun

; EXERCISE 2: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

;;;;;;;;
;; Auxiliary functions
;;;;;;;;
;; SET-IN-ROW: set the value of an element of a row 'r' at column 'col' with value 'val'
(defun set-in-row (r col val)
  (cond
    ((= col 0) (cons val (cdr r)))
    (T (cons (car r) (set-in-row (cdr r) (- col 1) val))) ))

;; SET-SQUARE: set the value of a square to 'val' which is located at ('row', 'col')
(defun set-square (s row col val) ; 
    (cond
        ((null s) nil)
        ((= row 0) (cons (set-in-row (car s) col val) (cdr s)))
        (T (cons (car s) (set-square (cdr s) (- row 1) col val))) ))


;; GET-IN-ROW: get the object at the column
(defun get-in-row (r col)
  (cond
    ((> col (length r)) 1)
    ((= col 0) (car r))
    (T (get-in-row (cdr r) (- col 1))) ))

;; GET-SQUARE: get the object on the position (square with coordinates)
(defun get-square (s row col)
  (cond
    ((> row (length s)) 1)
    ((= row 0) (get-in-row (car s) col))
    (T (get-square (cdr s) (- row 1) col)) ))


;; BOX-LEAVE: the helper function checks whether keeper push out a box from goal
(defun box-leave (s y x)
  (let ( (o (get-square s y x)) )
         (cond
            ((isBoxStar o) 6)
            (T 3) )         
  ) )
;; MOVE-A-BOX: if the box is moved then the keeper must move in the same direction 
;;                           and behind the box,
;;                           also if it is possible to move the box into new position, then keeper steps
;;                           into the old position of the box
;;                           the new position of the box is at (ny, nx), since we can only move at horizontal
;;                           or vertical direction, so at x or y is changed only one at a time.
;; Specification: keeper moves to box's old position, then move box to blank
;;                        same as above, except that now box will be in goal
;;                        only the above two case we can move a box, 
;;                        also no need to move a box already on goal
;; 1 - up
;; 2 - down
;; 3 - left
;; 4 - right
(defun move-a-box (s y x dir)
  (let* ( (ny (cond 
                      ((= dir 1) (- y 1))
                      ((= dir 2) (+ y 1))
                      (T y)) )
            (nx (cond
                      ((= dir 3) (- x 1))
                      ((= dir 4) (+ x 1))
                      (T x)) )
            (obj (get-square s ny nx)) )
          (cond
              ( (= obj 0) (set-square (set-square s y x (box-leave s y x)) ny nx 2) ) 
              ( (= obj 4) (set-square (set-square s y x (box-leave s y x)) ny nx 5) )    
              (T nil) )
    ))

;; KEEPER-LEAVE: check whether the keeper is leaving a goal, we must restore a goal at that position
(defun keeper-leave (s y x)
  (let ( (o (get-square s y x)) )
         (cond
            ((isKeeperStar o) 4) ; mark again the goal position while the keeper moves out of this position
            (T 0) )         
          ) )

;;;;;;;;;;;
;; TRY-MOVE: the function tries to move the keeper to his around.
;; 1 - up
;; 2 - down
;; 3 - left
;; 4 - right
;; the new position of keeper is at (ny, nx)
;; if the new position is blank then move directly,
;; if it is a box then try to move box while moves keeper 
;; (keeper will occupy the previous position of box)
;; if it is a goal then change the new position to "keeper + star"
;; here we can also push box out of the goal, so we also need to check this situation
;; only the above four cases the keeper can move into the new position
;; x - column of position of keeper
;; y - row       of position of keeper
(defun try-move (s dir)
  (let* ( (pos (getKeeperPosition s 0))
            (x (car pos))
            (y (cadr pos))
            (ny (cond 
                     ((= dir 1) (- y 1))
                     ((= dir 2) (+ y 1))
                     (T y)) )
            (nx (cond
                     ((= dir 3) (- x 1))
                     ((= dir 4) (+ x 1))
                     (T x)))
            (o (get-square s ny nx)) )
          (cond 
             ((= o 0) (set-square (set-square s y x (keeper-leave s y x)) ny nx 3))     
             ((= o 2) (set-square (move-a-box s ny nx dir) y x (keeper-leave s y x)))
             ((= o 5) (set-square (move-a-box s ny nx dir) y x (keeper-leave s y x)))
             ((= o 4) (set-square (set-square s y x (keeper-leave s y x)) ny nx 6))
             (T nil) )
  ))

;; 1 - up
;; 2 - down
;; 3 - left
;; 4 - right
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 1) (try-move s 2) (try-move s 3) (try-move s 4)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE 3: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE 4: Modify this function to compute the 
; number of misplaced boxes in s.
;
;; COUNT-BOX-ROW: count the number of boxes not on goal positions in a row
(defun count-box-row (r)
    (cond
         ((null r) 0)
         ((and (atom r) (isBox r)) 1)
         ((and (atom r) (not (isBox r))) 0) 
         (T (+ (count-box-row (car r)) (count-box-row (cdr r))) ) 
    ))

;; H1: the heuristic function
;; Question: is this heuristic admissible?
;; Answer:   This heuristic is indeed admissible since it cannot ever overestimate the cost of reaching the goal.
;; The function by nature at most can only report that all boxes are off goal spots, 
;; and thus the heuristic is admissible.

(defun h1 (s)
    (cond
        ((null s) 0)
        (T (+ (count-box-row (first s)) (h1 (rest s))))
    ))

; EXERCISE 5: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;; FIND-OBJECTS-HELPER: this function helps the function looking for specified object in a row
(defun find-objects-helper  (lst typ row col)
  (cond 
    ((null lst) nil)
    ((= typ (car lst)) (append (list (list col row)) (find-objects-helper (cdr lst) typ row (+ col 1))))
    (T (find-objects-helper (cdr lst) typ row (+ col 1) ) )
    ) )

;; FIND-OBJECTS: this function looks for all specified objects in the state
;; and it returns a list of positions (coordinates) of the objects specified
(defun find-objects (s typ row)
  (cond
    ((null s) nil)
    (T (append (find-objects-helper (car s) typ row 0) (find-objects (cdr s) typ (+ row 1) ) ) )
    ))

;; CENTROID-HELPER:
(defun centroid-helper (lst ysum xsum tol)
  (cond 
    ((null lst) nil)
    ((= (length lst) 1) (list (+ xsum (caar lst)) (+ ysum (cadar lst)) (+ tol 1)) )
    (T (centroid-helper (cdr lst) (+ ysum (cadar lst)) (+ xsum (caar lst)) (+ tol 1)))
    ))

;; CENTROID-OBJECTS: this function calculates the centroid of objects with the same type in a state
(defun centroid-objects (s typ)
  (let* ( (poslst (find-objects s typ 0)) 
            (center (cond
                            ((null poslst) nil)
                            (T (centroid-helper poslst 0 0 0)) ) ) )
    (cond
      ((null center) nil)
      (T (list (floor (/ (first center) (third center) )) (floor (/ (second center) (third center))) (third center) ))
  ) ))

;; MANHATTAN: this function calculates the manhattan distance between to positions (coordinates)
(defun manhattan (p1 p2)
  (let* ( (xd (cond 
                ((> (first p1) (first p2)) (- (first p1) (first p2)))
                (T (- (first p2) (first p1))) ))
            (yd (cond 
                ((> (second p1) (second p2)) (- (second p1) (second p2)))
                (T (- (second p2) (second p1))) )) )
          (+ yd xd) ))

;; H404246421: this function is the heuristic to be used by A*
;; the algorithm is that we need to get the effort of moving from position of keeper to the centroid of
;; boxes which is the manhattan distance; then we calculte the manhattan distance between the centroid of boxes not in goal and 
;; the centroid of all goals not occupied.
;; the manhattan distance of two centroids then multiply by the number of boxes, this is the least effort must be taken by
;; the keeper to move all the boxes into goal. 
;; Here instead of calculating the nearest goal for each box, we compute the distance of centroids to avoid case that two or more boxes have 
;; the goal position with minimum distance.
;; we can prove that this heuristic is admissible, also the calculation of centroid only consider the boxes not in goal and the goal not
;; occupied by boxes.
;; one thing maybe need to pay attention is that the region cover by all boxes may overlops with
;; the region covered by all the goals (for example, the circle formed by goals may be inside the circle formed by boxes), in such case,
;; the heuristic may be similiar to h0.
(defun hhh (s)
  (let* ((keeperpos (getKeeperPosition s 0))
           (centroidbox  (centroid-objects s 2))
           (centroidgoal (centroid-objects s 4)) 
           (manhattankb ( manhattan keeperpos (list (first centroidbox) (second centroidbox)) ) ) 
           (manhattanbg ( manhattan (list (first centroidbox) (second centroidbox)) (list (first centroidgoal) (second centroidgoal)) ) ) )
    (+ manhattankb (* manhattanbg (third centroidbox)))
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

; (a* p1 #’goal-test #’next-states #’h1)
