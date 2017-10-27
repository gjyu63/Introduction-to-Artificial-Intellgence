;PAD: Takes a single non-negative number (not too large) as argument
;Return nth Padovan number

;explain: use cond to deal with base cases (0, 1 and 2) and general cases (specified in hw specs)
(defun PAD (n)
        (cond   ((= n 0) 1)
                ((= n 1) 1)
                ((= n 2) 1)
                ;general case
                (t (+ (PAD (- n 2)) (PAD (- n 3))))
        )

)

;SUM: Takes a single non-negative number as argument
;Return number of addition op. required to calculate pad(n) (last problem)

;explain: deal with base cases, and notice that sum(n) = sum(n-2) + sum(n-3) +1, we use this
;to calculate sum(n)

(defun SUM (n)
        (cond   ((= n 0) 0)
                ((= n 1) 0)
                ((= n 2) 0)
                ;general case
                (t (+ (SUM (- n 2)) (SUM (- n 3)) 1))
        )
)

;ANON: Replace every symbol in a tree with ?
;Takes a List as an argument. List represents a tree

;explain: recursively deal with the first child and the other children of the given root
;then combine them as a list, we get one level of a tree

(defun ANON (L)
        (cond ((null L) ()) ;if tree's nothing, return a empty list
                ((atom L) '?) ;replace leaves with ?
                (T (cons (ANON (car L)) (ANON (cdr L)))) ;recursive call specified on the top
		)
)