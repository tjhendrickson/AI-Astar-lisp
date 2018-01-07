; Author - Timothy Hendrickson
; Date - December 7, 2017

; a-star algorithm implementation with cannibals and missionaries problem state space

; generate lisp structure "problem" similar to problem formulation set up pg 66
(defstruct problem initial-state goal-state node-expansions)

; generate lisp structure "node" similar to pg 78, this will have to be a stack
(defstruct nodes states parent actions path-costs)

; generate lisp structure expanded nodes to keep a running tab of expanded nodes in order to avoid repeat expansions
(defstruct expanded node)

;initialize expanded nodes
(setf node-expansions '())
			
(defun a-star-search (problem)
	
	; initialize relevant variables
	
	; set node expansion to one
	(setf (problem-node-expansions problem) 1)
	
	; initialize problem and determine possible nodes
	(setf init-problem (successors problem (problem-initial-state problem)))
	
	; initialize frontier stack
	(setf frontier '())
	
	 ;push frontier of initial state to stack frontier
	(let
		((x (problem-node-expansions problem))
		(y init-problem))
		(push (format t "node expansion #: ~D~%" x) frontier)
		(push (format t "~S~%" y) frontier)
		(push (format t "~%") frontier)
	)
	; call general search function with nodes sorted by f cost
	(general-search (queue-f-cost init-problem) problem)
)

(defun general-search (sorted-frontier problem)

	; if queue is empty return nil
	(if (null sorted-frontier)
		(progn
			(format t "No nodes to search, must exit")
			(return nil)
		)
	)
	
	; remove to be expanded node from stack
	(setf node (pop sorted-frontier))
	
	; if node is equal to goal state the problem completed successfully
	(if (equalp (problem-goal-state problem) (nodes-states node))
		(progn
			;print success and goal node to clisp interpreter
			;print success and goal node to clisp interpreter and return frontier stack
			(format t "Success")
			(print (nodes-states node))
			(return frontier)
			;(return nil)
		)
	)
	
	; avoid state if it has been expanded previously
	(if (not (null node-expansions))
		(progn
			(loop for ii from 0 to (- (length node-expansions) 1) doing
				(let
					((expand-node (elt node-expansions ii)))
					; if this node has been expanded previously do not expand node,
					; and leave it removed from queue and re-run general search					
					(if (equalp (expanded-node expand-node) (nodes-states node))
						(general-search (queue-f-cost sorted-frontier) problem)
					)
				)
			)
		)
	)
	
	; add node to expanded nodes
	(push (make-expanded :node (nodes-states node)) node-expansions)
	
	; increase node expansion by one
	(setf (problem-node-expansions problem) (+ (problem-node-expansions problem) 1))
	
	; expand node 
	(setf children (successors problem (nodes-states node)))
	
	; add children of expanded node to frontier while avoiding parents and repeat nodes		
	(loop while (not (null children)) doing
		(progn
			(setf child (pop children))
			(setf counter 0)
			(loop for jj from 0 to (-(length sorted-frontier)1) doing
				(let
					((node (elt sorted-frontier jj)))
					(if (or (equalp (nodes-parent node) (nodes-states child)) (equalp (nodes-states node) (nodes-states child)))
						(return)
					)
					(setf counter (+ counter 1))
				)
			)
			(if (equalp counter (length sorted-frontier))
				(if (not (equalp (nodes-states child) (nodes-parent node)))
						(push child sorted-frontier)
				)
			)
		)
	)
	
	; push frontier of initial state to stack frontier
	(let
		((x (problem-node-expansions problem))
		(y sorted-frontier))
		(push (format t "node expansion #: ~D~%" x) frontier)
		(push (format t "~S~%" y) frontier)
		(push (format t "~%") frontier)
	)
	
	; run general search again with new frontier sorted by the f-cost
	(general-search (queue-f-cost sorted-frontier) problem)
)

;define f-cost for current state
(defun f-cost (current-state problem)
	(setf h-cost 0)
	(setf f-cost 0)
	(setf g-cost 0)
	(let
		; h-cost can be the the number of people on the initial side / the boat capacity
		((h-cost (/ (+ (elt current-state 0) (elt current-state 1)) (cannibal-problem-boat-capacity problem)))
		; g-cost is the number of boat crossings
		 (g-cost (cannibal-problem-node-expansions problem)))
		(return-from f-cost (+ h-cost g-cost))
	)
)

; sort nodes based on f-cost 
(defun queue-f-cost (q)
	(setf sorted-nodes '())
	(setf path-costs '())
	
	;extract path costs from nodes
	(loop for ii from 0 to (- (length q)1) doing
		(let
			((node (elt q ii)))
			; create vector linking path-cost to state
			(push (list ii (nodes-path-costs node)) path-costs)
		)
	)
	
	;sort nodes based on path-costs and return new queue
	(setf path-costs (sort (copy-list path-costs) #'> :key #'second))

	(loop while (not (null path-costs)) doing
		
		(setf path-cost (pop path-costs))
		(let
			((node (elt q (car path-cost))))
			(push node sorted-nodes)
		)
	)
	(return-from queue-f-cost sorted-nodes)
)

