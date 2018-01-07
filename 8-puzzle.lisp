; Author - Timothy Hendrickson
; Date - December 7, 2017

; load needed script/s
(load "a_star_search_8_puzzle.lsp")

;Generate variables for initial and goal state and node-expansions for 8 puzzle problem
(defparameter *first-state* (make-array '(3 3) :initial-contents '((0 1 3) (4 2 5) (7 8 6))))

;example of unsolveable puzzle
;(defparameter *first-state* (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (8 7 0))))

(defparameter *goal-state* (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 0))))

;create intermediate array for state creation
(defun create-state (matrix)
	(setf state (make-array '(3 3)))
	(loop named outfer for a from 0 to 2 do
		(loop for b from 0 to 2
				for s = (aref matrix a b)
				do (setf (aref state a b) s)))
	(return-from create-state state)
)

;define f-cost for current stateso
(defun f-cost (current-state initial-state goal-state)
	(setf h 0)
	(setf f 0)
		(loop for a from 0 to 2 doing
			(loop for b from 0 to 2 collecting
				(progn
					(let
					; if a tile in the current state and that same tile location in the goal-state differ add one to h
					((state-square (aref current-state a b))
					(goal-square (aref goal-state a b)))
					(if (not (equalp state-square goal-square))
						(setf h (+ h 1)))				
					)
				)
		
			)
		)
		
	;find blanks from initial-state and current-state
	(setf initial-state-blank (find-empty initial-state))
	(setf current-state-blank (find-empty current-state))
	;now determine how many steps it would take to get from initial to current state
	(let 
		((x (- (aref current-state-blank 0) (aref initial-state-blank 0)))
		 (y (- (aref current-state-blank 1) (aref initial-state-blank 1))))
		(setf x (abs x))
		(setf y (abs y))
		(setf g (+ x y)))
	;return f-cost from function
	(return-from f-cost (+ h g))
)

;determine feasible node-expansions, actions, and path costs while avoiding initial state
(defun define-problem (position current-state)
	(setf branches '())
	
	;determine possible actions and legal moves
	(if (not (equal (aref position 0) 0))
		(progn
		(setf state (create-state current-state))
		(let 
			((row (- (aref position 0) 1))
			(column (aref position 1)))
			; where is blank position?
			(setf what-is-it (aref current-state row column))
			; change blank state
			(setf (aref state row column) 0) 
			(setf (aref state (aref position 0) (aref position 1)) what-is-it)
			(if (not (equalp state (problem-initial-state 8-puzzle)))
				(progn
					(setf cost (f-cost state (problem-initial-state 8-puzzle) (problem-goal-state 8-puzzle)))	
					(push (make-nodes :states state :actions 'up :path-costs cost :parent current-state) branches))
			)
		)
		)
	)

	(if (not (equal (aref position 1) 0))
		(progn				
		(setf state (create-state current-state))	
		(let 
			((row (aref position 0))
			(column (- (aref position 1) 1)))
			(setf what-is-it (aref current-state row column))
			(setf (aref state row column) 0) 
			(setf (aref state (aref position 0) (aref position 1)) what-is-it)
			(if (not (equalp state (problem-initial-state 8-puzzle)))
				(progn
					(setf cost (f-cost state (problem-initial-state 8-puzzle) (problem-goal-state 8-puzzle)))					
					(push (make-nodes :states state :actions 'left :path-costs cost :parent current-state) branches))
			)
		)
		)
	)
	
	(if (not (equal (aref position 0) 2))
		(progn			
		(setf state (create-state current-state))
		(let 
			((row (+ (aref position 0) 1))
			(column (aref position 1)))  
			(setf what-is-it (aref state row column))
			(setf (aref state row column) 0) 
			(setf (aref state (aref position 0) (aref position 1)) what-is-it)
			(if (not (equalp state (problem-initial-state 8-puzzle)))
				(progn
					(setf cost (f-cost state (problem-initial-state 8-puzzle) (problem-goal-state 8-puzzle)))
					(push (make-nodes :states state :actions 'down :path-costs cost :parent current-state) branches))
			)
		)
		)
	)

	(if (not (equal (aref position 1) 2))
		(progn
		(setf state (create-state current-state))
		(let 
			((row (aref position 0)) 
			(column (+ (aref position 1) 1)))
			(setf what-is-it (aref state row column))
			(setf (aref state row column) 0) 
			(setf (aref state (aref position 0) (aref position 1)) what-is-it)
			(if (not (equalp state (problem-initial-state 8-puzzle)))
				(progn
					(setf cost (f-cost state (problem-initial-state 8-puzzle) (problem-goal-state 8-puzzle)))
					(push (make-nodes :states state :actions 'right :path-costs	cost :parent current-state) branches))
			)
		)
		)
	)
	; return children node from function
	(return-from define-problem branches)
)

;find empty space/blank (represented by a 0)
(defun find-empty (matrix)
	(loop named outfer for a from 0 to 2 do
		(loop for b from 0 to 2
		for s = (aref matrix a b)
			if (= s 0) 
				do (setf position (vector a b))
	(return-from find-empty position))))

;before running through a* algorithm determine if the initial state is feasible
(defun solvable? (initial-state)
	(setf inversion 0)
		(loop for a from 0 to 7 doing
			(let
				((b (+ a 1)))
				(let
					((j (row-major-aref initial-state a))
					(i (row-major-aref initial-state b)))
					(if (and (not (equalp j 0)) (not (equalp i 0)))
						(if (< i j) 
							(setf inversion (+ inversion 1))
						)
					)
				)
			)
		)
	(return-from solvable? inversion)
)


; set up 8 puzzle problem by defining structure
(setf 8-puzzle (make-problem :initial-state *first-state* :goal-state *goal-state*))

(if (evenp (solvable? (problem-initial-state 8-puzzle)))
	(a-star-search 8-puzzle)
(format t "Problem not solveable"))










