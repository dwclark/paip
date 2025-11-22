(in-package :paip-eliza)

(declaim (optimize (debug 3)))

(defstruct rule pattern responses)

(defun variable-p (sym)
  (if (symbolp sym)
      (let* ((str (symbol-name sym))
	     (pos (position #\? str)))
	(and pos
	     (= 0 pos)
	     (< 1 (length str))))
      nil))

(defun starts-same (left right)
  (cond ((null left)
	 (values t right))
	((null right)
	 (values nil nil))
	((not (equal (first left) (first right)))
	 (values nil nil))
	(t
	 (starts-same (rest left) (rest right)))))

(defun greedy-exact-match (starting the-list)
  (let ((so-far nil)
	(success nil)
	(pre-match nil)
	(post-match nil))
    (loop for sub on the-list
	  do (multiple-value-bind (matched post) (starts-same starting sub)
	       (if matched
		   (setf success t pre-match (reverse so-far) post-match post)))
	     (push (car sub) so-far))
    (if success
	(values success pre-match post-match)
	(values success nil nil))))

(defun pattern-match (patterns input)
  (loop with prev = nil
	with ret = nil
	with next = input
	for pattern in patterns
	do (cond ((variable-p pattern)
		  (setf prev pattern)
		  (push (cons pattern next) ret))
		 ((listp pattern)
		  (multiple-value-bind (success pre post) (greedy-exact-match pattern next)
		    (if success
			(progn
			  (if prev
			      (rplacd (assoc prev ret) pre))
			  (setf prev nil next post))
			(return nil))))
		 (t (return nil)))
	finally (return (reverse ret))))

(defun eliza-rules ()
  (list
   (make-rule :pattern '(?x (hello) ?y)
	      :responses '((How do you do. Please state your problem.)))
   (make-rule :pattern '(?x (I want) ?y)
	      :responses '((What would it mean if you got ?y ?)
			   (Why do you want ?y ?)
			   (Suppose you got ?y soon)))
   (make-rule :pattern '(?x (if) ?y)
	      :responses '((Do you really think its likely that ?y ?) (Do you wish that ?y ?)
			   (What do you think about ?y ?) (Really-- if ?y)))
   (make-rule :pattern '(?x (no) ?y)
	      :responses '((Why not?) (You are being a bit negative)
			   (Are you saying "NO" just to be negative?)))
   (make-rule :pattern '(?x (I was) ?y)
	      :responses '((Were you really?) (Perhaps I already knew you were ?y ?)
			   (Why do you tell me you were ?y now?)))
   (make-rule :pattern '(?x (I feel) ?y)
	      :responses '((Do you often feel ?y ?)))
   (make-rule :pattern '(?x (I felt) ?y)
	      :responses '((What other feelings do you have?)))))

(defparameter *eliza-rules* nil)

(defun find-matching (input)
  (loop for rule in *eliza-rules*
	do (let ((variables (pattern-match (rule-pattern rule) input)))
	     (if variables
		 (return (values variables (rule-responses rule)))))
	finally (return (values nil nil))))

(defun change-perspective (lst)
  (sublis '((i . you) (you . i) (me . you) (am . are) (myself . yourself) (yourself . myself)) lst))

(defun make-substitutions (variables response)
  (let* ((subs (remove-if-not #'(lambda (cell) (cdr cell)) variables))
	 (new-perspective-subs (mapcar #'(lambda (cell) (cons (car cell) (change-perspective (cdr cell)))) subs)))
    (flatten (sublis new-perspective-subs response))))

(defun print-response (&optional the-list)
  (if the-list
      (format t "~{~A ~}" the-list)
      (format t "WHAT DO YOU MEAN?")))

(defun eliza (&optional (the-rules (eliza-rules)))
  (let ((*eliza-rules* the-rules))
    (loop do
      (print 'eliza>)
      (let ((patient-said (read-from-string (concatenate 'string "(" (read-line) ")"))))
	(if (equal '(bye) patient-said)
	    (progn
	      (format t "BYE!")
	      (return))
	    (multiple-value-bind (variables possible) (find-matching patient-said)
	      (if variables
		  (let* ((index (random (length possible)))
			 (eliza-response (make-substitutions variables (elt possible index))))
		    (print-response eliza-response))
		  (print-response))))))))
