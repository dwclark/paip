(defparameter fail nil "Indicates pat-match failure")

(defparameter no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defun simple-equal (x y)
  "Are x and y equal?  (Don't check inside strings.)"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
           (simple-equal (rest x) (rest y)))))

(defun variable-p (x)
  "Is X a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
   "Add a (var . value) pair to a binding list. "
   (cons (cons var val)
	 ;; Once we add a "real" binding,
	 ;; we can get rid of the dummy no-bindings
	 (if (eq bindings no-bindings)
	     nil
	     bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
   "Match pattern against input in the context of the bindings"
   (cond ((eq bindings fail) fail)

         ((variable-p pattern)
          (match-variable pattern input bindings))

         ((eql pattern input) bindings)
         
         ((segment-pattern-p pattern)                ; ***
          (segment-match pattern input bindings))    ; ***

         ((and (consp pattern) (consp input))
          (pat-match (rest pattern) (rest input)
                     (pat-match (first pattern) (first input)
                                bindings)))

         (t fail)))

(defun starts-with (pattern look-for)
  (and (consp pattern)
       (eq (first pattern) look-for)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))

          ((equal input (binding-val binding)) bindings)

          (t fail))))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))
