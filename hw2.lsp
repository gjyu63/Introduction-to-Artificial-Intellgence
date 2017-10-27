;;;;;;;;;;;;;;
; Homework 2 
;Jiayu Guo
;304-773-262
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
;INPUT: A list, representing a fringe of a three
;output: a list with leaf nodes of the tree, in bfs order 
(defun BFS (FRINGE)
    (cond   ((null FRINGE) nil);base case
            ;case: if the car of fringe is a leaf, then put it to the list, and bfs on the rest of the fringe
            ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
            (t (BFS (append (cdr FRINGE) (car FRINGE))))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
;check if equal
    (cond   ((equal S '(T T T T)) t)
            (t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
;to check whether the input state, S, is illegal, according to the game rule
  (cond ((and (not (equal (first S) (second S))) (equal (second S) (third S))) nil) 
        ((and (not (equal (first S) (second S))) (equal (second S) (fourth S))) nil)
;to check if the action is legal, with the consideration of 3 cases: move h, b or d
;move homer himself, and make sure baby and dog are not at the same side
        ((and (equal A 'h) (not (or (and (equal (second S) (third S)) (equal (first S) (second S)))
        (and (equal (first S) (second S)) (equal (second S) (fourth S)))))) 
            (list (list (not (first S)) (second S) (third S) (fourth S))))
;move homer with baby, make sure they were at the same side    
        ((and (equal A 'b) (equal (first S) (second S))) 
            (list (list (not (first S)) (not (second S)) (third S) (fourth S))))
;move homer with dog, make sure baby don't have the access to posion
        ((and (equal A 'd)(and (equal (first S) (third S))(not (equal (second S) (fourth S))))) 
            (list (list (not (first S)) (second S) (not (third S)) (fourth S))))
;move homer with posion, similarly, make sure dog won't eat baby
        ((and (equal A 'p)(and (equal (first S) (fourth S))(not (equal (second S) (third S))))) 
            (list (list (not (first S)) (second S) (third S) (not (fourth S)))))
;if nothing could happen, this route is dead
        (T nil)))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
;append possible states to a list
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
;recursively check if s is included in states
    (cond   ((= (length STATES) 0) nil)
            ((equal (car STATES) S) t)
            (t (ON-PATH S (cdr STATES)))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond   ((= (length STATES) 0) nil);base case
            ((DFS (car STATES) PATH) (DFS (car STATES) PATH));if this path can return final state, return it
            (t (MULT-DFS (cdr STATES) PATH)));otherwise recursively call the remained states

)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond   ((FINAL-STATE S) (append PATH (list S)));base case, if has been arrived
            ;make sure the state has not been visited
            ;then call mult-dfs and pass it with successer function
            ((not (ON-PATH S PATH)) (MULT-DFS (SUCC-FN S) (append PATH (list S))))
            (t nil)))
