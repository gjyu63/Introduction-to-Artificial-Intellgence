

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Homework 2                 ;
; Feng Shi (UID 404-246-421) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; function BFS (FRINGE: list): list
; returns  list
;
; Description:
;  this function uses the breadth first search
;  to generate a flattened list
;  the argument FRINGE here is working as a FIFO queue 
(defun BFS (FRINGE)
  (cond 
    ((null FRINGE) nil)
    ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
    (T (BFS (append (cdr FRINGE) (car FRINGE))))
    ))

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
  (cond 
    ((equal S '(T T T T))  T)
      (T nil))) ; anything not equal to final state returns NIL


(defun NEXT-STATE (S A)
  (cond
    ; this part is for verifying whether the input state is illegal:
    ; baby and poison, or baby and dog at the same side of bank without Homer presenting
    ; is forbidden 
    ((and (not (equal (first S) (second S))) (equal (second S) (third S))) nil) 
    ((and (not (equal (first S) (second S))) (equal (second S) (fourth S))) nil)
    ; the following part is for detecting whether the action is legal,
    ; by following the same rule not letting baby stay alone with dog, or baby alone with poison.
    ((and (equal A 'h) 
          (not (or (and (equal (first      S) (second S)) 
                        (equal (second S) (third    S)))
                   (and (equal (first      S) (second S)) 
                        (equal (second S) (fourth  S)))))) (list (list (not (first S)) (second S) (third S) (fourth S))))
    ((and (equal A 'b) (equal (first S) (second S))) 
          (list (list (not (first S)) (not (second S)) (third S) (fourth S))))
    ((and (equal A 'd) 
          (and (equal (first S) (third S)) 
               (not (equal (second S) (fourth S))) )) 
          (list (list (not (first S)) (second S) (not (third S)) (fourth S))))
    ((and (equal A 'p) 
          (and (equal (first S) (fourth S)) 
               (not (equal (second S) (third S))) )) 
          (list (list (not (first S)) (second S) (third S) (not (fourth S)))))
    (T nil)))


(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))
    ; append all possible states expending from input state.


(defun ON-PATH (S STATES)
  (cond 
    ((= (length STATES) 0) nil)
    ((equal s (car STATES)) T)
    (T (ON-PATH S (cdr STATES)))))

(defun MULT-DFS (STATES PATH)
  (cond 
    ((= (length STATES) 0) nil) ; return nil if there’s no more possible successor states to look at
    ((DFS (car STATES) PATH) (DFS (car STATES) PATH)) ;
    ; depth-first search on first state, if it can reach the final state, return it.
    (T (MULT-DFS (cdr STATES) PATH)) ))
    ; otherwise, ignore this branch and call recursively mult-dfs on the rest states.


(defun DFS (S PATH)
  (cond 
    ((FINAL-STATE S) (append PATH (list S)))
    ; if current state s has not been visited in path, we visit it, 
    ; running dfs on it by first calling mult-dfs
    ((not (ON-PATH S PATH)) (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    ; the state has been visited and is on path, no need to proceed.
    (T nil)))
