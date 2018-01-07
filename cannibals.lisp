; Author - Timothy Hendrickson
; Date - December 7, 2017

; missionaries and cannibals state space, modified from Russell and Norvig. 

;; load needed script a_star_search_cannibals.lsp
(load "a_star_search_cannibals.lsp")

;;;; The Missionaries and Cannibals Domain

(defstruct (cannibal-problem (:include problem)) boat-capacity)

(defmethod successors ((problem cannibal-problem) state)
	"Return a list of (action . state) pairs.  An action is a triple of the
	form (delta-m delta-c delta-b), where a positive delta means to move from
	side 1 to side 2; negative is the opposite.  For example, the action (1 0 1)
	means move one missionary and 1 boat from side 1 to side 2."
	(let ((pairs nil)
		(nodes nil))
		
		; all possible actions in a list, with a boat capacity of 6. Perhaps not the most elegant...
		(loop for action in '(
			(+6 0 +1) (0 +6 +1) (+5 +1 +1) (+1 +5 +1) (+4 +2 +1) (+2 +4 +1) (+3 +3 +1) 
			(+5 0 +1) (0 +5 +1) (+4 +1 +1) (+1 +4 +1) (+3 +2 +1) (+2 +3 +1)
			(+4 0 +1) (0 +4 +1) (+3 +1 +1) (+1 +3 +1) (+2 +2 +1)
			(+3 0 +1) (0 +3 +1) (+2 +1 +1) (+1 +2 +1)
			(+2 0 +1) (0 +2 +1) (+1 +1 +1)
			(+1 0 +1) (0 +1 +1)
			(-6 0 -1) (0 -6 -1) (-5 -1 -1) (-1 -5 -1) (-4 -2 -1) (-2 -4 -1) (-3 -3 -1) 
			(-5 0 -1) (0 -5 -1) (-4 -1 -1) (-1 -4 -1) (-3 -2 -1) (-2 -3 -1)
			(-4 0 -1) (0 -4 -1) (-3 -1 -1) (-1 -3 -1) (-2 -2 -1)
			(-3 0 -1) (0 -3 -1) (-2 -1 -1) (-1 -2 -1)
			(-2 0 -1) (0 -2 -1) (-1 -1 -1)
			(-1 0 -1) (0 -1 -1)) do
			(let ((new-state (take-the-boat state action)))
				(when (and new-state (not (cannibals-can-eat? new-state)))
					(push (cons action new-state) pairs)
					(let ((cost (f-cost new-state problem)))
						(push (make-nodes :states new-state :parent state :actions action :path-costs cost) nodes)	
					)
				)
			)
		)
	(return-from successors nodes))
)

(defstruct (cannibal-state (:conc-name nil) (:type list))
  "The state says how many missionaries, cannibals, and boats on each
  side.  The components m,c,b stand for the number of missionaries,
  cannibals and boats, respectively, on the first side of the river.
  The components m2,c2,b2 are for the other side of the river."
  (m1 3) (c1 3) (b1 1) (m2 0) (c2 0) (b2 0))

(defun take-the-boat (state action)
  "Move a certain number of missionaries, cannibals, and boats (if possible)."
  (destructuring-bind (delta-m delta-c delta-b) action
    (if (or (and (= delta-b +1) (> (b1 state) 0))
	    (and (= delta-b -1) (> (b2 state) 0)))
	(let ((new (copy-cannibal-state state)))
	  (decf (m1 new) delta-m) (incf (m2 new) delta-m)
	  (decf (c1 new) delta-c) (incf (c2 new) delta-c)
	  (decf (b1 new) delta-b) (incf (b2 new) delta-b)
	  (if (and (>= (m1 new) 0) (>= (m2 new) 0)
		   (>= (c1 new) 0) (>= (c2 new) 0))
	      new
	    nil)
	   )
      nil)
	)
)

(defun cannibals-can-eat? (state)
  "The cannibals feast if they outnumber the missionaries on either side."
  (or (> (c1 state) (m1 state) 0)
      (> (c2 state) (m2 state) 0)
  )
)

; 15 by 15 cannibals problem
(setf cannibal-15 (make-cannibal-problem :initial-state (make-cannibal-state :m1 15 :c1 15 :b1 1 :m2 0 :c2 0 :b2 0) 
:goal-state (make-cannibal-state :m1 0 :c1 0 :b1 0 :m2 15 :c2 15 :b2 1) :boat-capacity 6))

; run cannibals problem through a* algorithm
(a-star-search cannibal-15)

; 24 by 24 cannibals problem
;(setf cannibal-24 (make-cannibal-problem :initial-state (make-cannibal-state :m1 24 :c1 24 :b1 1 :m2 0 :c2 0 :b2 0) 
;:goal-state (make-cannibal-state :m1 0 :c1 0 :b1 0 :m2 24 :c2 24 :b2 1) :boat-capacity 6))

; run cannibals problem through a* algorithm
;(a-star-search cannibal-24)


