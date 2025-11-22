(in-package :paip-gps)

(defvar *ops* nil)

(defstruct op action conds add del)

(defstruct state current actions past)

(defun has-goal-p (state goal)
  (member goal (state-current state) :test #'equal))

(defun has-goals-p (state goals)
  (every #'(lambda (goal)
	     (has-goal-p state goal))
	 goals))

(defun already-achieved-p (state goal)
  (member goal (state-past state) :test #'equal))

(defun achieves-goal-p (op goal)
  (member goal (op-add op) :test #'equal))

(defun op-apply (op state)
  (loop with tmp-state = state
	for goal in (op-conds op)
	do (setf tmp-state (if tmp-state (achieve goal tmp-state) nil))
	finally (progn
		  (if (and tmp-state (has-goals-p tmp-state (op-conds op)))
		      (setf (state-current state) (set-difference (state-current state) (op-del op) :test #'equal)
			    (state-current state) (union (state-current state) (op-add op) :test #'equal)
			    (state-actions state) (append (state-actions state) (list (op-action op)))
			    (state-past state) (append (state-past state) (list goal)))
		      (setf tmp-state nil))
		  (return tmp-state))))

(defun achieve (goal state)
  (cond ((has-goal-p state goal)
	 state)
	((already-achieved-p state goal)
	 nil)
	(t
	 (loop with tmp-state = state
	       for op in (remove-if-not #'(lambda (op) (achieves-goal-p op goal)) *ops*)
	       do (setf tmp-state (if tmp-state (op-apply op tmp-state) nil))
		  (if tmp-state
		      (return-from achieve tmp-state)))
	 nil)))

(defun gps (have goals)
  "Try and achieve all goals. If state is not null and all goals are in current state, we have a solution"
  (let ((state (make-state :current have :actions nil :past nil)))
    (loop for goal in goals
	  do (setf state (if state (achieve goal state) nil))
	  finally (return (if (and state (has-goals-p state goals))
			      state
			      nil)))))

(defun school-ops ()
  (list
   ;;top taxi op is added to test if my implementation has the same
   ;;issue as Norvig's does in 4.16. It does not.
   ;;This is a frustration with this book, it shows bad implementations
   ;;of bad algorithms. Thus it's hard to tell which is which. Since
   ;;my version solves the puzzle in 4.16, it's Norvig's implementation.
   (make-op 'taxi-son-to-school
	    :conds '(son-at-home have-money)
	    :add '(son-at-school)
	    :del '(son-at-home have-money))
   (make-op :action 'drive-son-to-school
	    :conds '(son-at-home car-works)
	    :add '(son-at-school)
	    :del '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :conds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add '(car-works))
   (make-op :action 'tell-shop-problem
	    :conds '(in-communication-with-shop)
	    :add '(shop-knows-problem))
   (make-op :action 'telephone-shop
	    :conds '(know-phone-number)
	    :add '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :conds '(have-phone-book)
	    :add '(know-phone-number))
   (make-op :action 'give-shop-money
	    :conds '(have-money)
	    :add '(shop-has-money)
	    :del '(have-money))))

(defun banana-ops ()
  (list
   (make-op :action 'climb-on-chair
	    :conds '(chair-at-middle-room at-middle-room on-floor)
	    :add '(at-bananas on-chair)
	    :del '(at-middle-room on-floor))
   (make-op :action 'push-chair-from-door-to-middle-room
	    :conds '(chair-at-door at-door)
	    :add '(chair-at-middle-room at-middle-room)
	    :del '(chair-at-door at-door))
   (make-op :action 'walk-from-door-to-middle-room
	    :conds '(at-door on-floor)
	    :add '(at-middle-room)
	    :del '(at-door))
   (make-op :action 'grasp-bananas
	    :conds '(at-bananas empty-handed)
	    :add '(has-bananas)
	    :del '(empty-handed))
   (make-op :action 'drop-ball
	    :conds '(has-ball)
	    :add '(empty-handed)
	    :del '(has-bal))
   (make-op :action 'eat-bananas
	    :conds '(has-bananas)
	    :add '(empty-handed not-hungry)
	    :del '(has-bananas hungry))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (make-op :action `(move from ,here to ,there)
	   :conds `((at ,here))
	   :add `((at ,there))
	   :del `((at ,here))))

(defun make-maze-pairs (pair)
  "Make maze ops in both directions"
  (list
   (make-maze-op (first pair) (second pair))
   (make-maze-op (second pair) (first pair))))

(defun make-maze-ops ()
  (mapcan #'make-maze-pairs
	  '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
	    (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
	    (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))
