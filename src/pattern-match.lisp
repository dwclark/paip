(in-package :paip)

(defparameter *variable-first* #\?)
(defparameter *variable-segment* "?*")
(defparameter *match-exact* '(t t))

(defun symbol-variable-p (s)
  (char= (char (symbol-name s) 0) *variable-first*))

(defun simple-matcher-p (p)
  (and (not (null p))
       (or (numberp p)
           (and (symbolp p)
                (not (symbol-variable-p p))))))

(defun variable-matcher-p (p)
  (and (symbolp p) (symbol-variable-p p)))

(defun segment-matcher-p (p)
  (and (listp p)
       (= 2 (length p))
       (search *variable-segment* (symbol-name (first p)))))

(defun segment-variable (p) (second p))

(defun variables-matched (pattern positions input)
  (loop for p in pattern
        for pos across positions
        for i from 0 below (length positions)
        collecting (progn
                     (cond ((simple-matcher-p p) *match-exact*)
                           
                           ((variable-matcher-p p)
                            (append (list p) (subseq input pos (1+ pos))))
                           
                           ((segment-matcher-p p)
                            (let ((next-index (if (= (1+ i) (length positions))
                                                  (length input)
                                                  (aref positions (1+ i)))))
                              (append (list (segment-variable p))
                                      (subseq input pos next-index)))))) into matched

        finally (return (let ((de-duped (remove-duplicates matched)))
                          (if (< 1 (length de-duped))
                              (remove *match-exact* de-duped)
                              de-duped)))))
                              

(defun pattern-match (pattern input)
  (let* ((matches-needed (length pattern))
         (positions (make-array matches-needed :fill-pointer 0))
         (input-pos -1)
         (next-pattern pattern)
         (next input)
         (matching-all nil))

    (flet ((update-match-state (ma)
             (vector-push (incf input-pos) positions)
             (setf next (rest next))
             (setf next-pattern (rest next-pattern))
             (setf matching-all ma)))
      
      (loop while (not (null next))
            do (let ((p (first next-pattern)))
                 (cond ((and (simple-matcher-p p) (eq p (first next)))
                        (update-match-state nil))

                       ((variable-matcher-p p)
                        (update-match-state nil))
                       
                       ((segment-matcher-p p)
                        (update-match-state t))

                       (matching-all
                        (incf input-pos)
                        (setf next (rest next)))
                       
                       (t (return-from pattern-match nil)))))
      
      (if (and (= matches-needed (length positions))
               (null next))
          (variables-matched pattern positions input)
          nil))))
