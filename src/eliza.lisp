(in-package :paip)

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would i t mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really ?) (Perhaps I already knew you were ?y)
     (Why do you t e l l me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pattern-match (rule-pattern rule) input)))
              (if (not (null result))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun clean-bad-chars (line)
  (let ((ret line))
    (setf ret (nsubstitute #\space #\" ret))
    (setf ret (nsubstitute #\space #\, ret))))

(defun read-next-response ()
  (let ((line (clean-bad-chars (read-line))))
    (read-from-string (concatenate 'string "(" line ")"))))

(defun format-reply (lst)
  (format nil "~{~A~^ ~}" lst)) 

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (let ((response (read-next-response)))
      (when (and (= 1 (length response))
                 (symbol-match 'bye (first response)))
        (write-line "bye!")
        (return-from eliza))
      (write-line (format-reply (flatten (use-eliza-rules response)))))))


