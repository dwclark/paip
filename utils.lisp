(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))
