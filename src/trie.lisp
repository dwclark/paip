(in-package :paip-trie)

(declaim (optimize (debug 3) (safety 3)))

(defstruct trie-node
  (key nil)
  (value nil)
  (children (make-array 0 :element-type 'trie-node :adjustable t :fill-pointer t) :type (vector trie-node *)))

(defstruct trie
  (pred< #'char<)
  (size 0)
  (root (make-trie-node)))

(defun binary-search (ary key pred<)
  (let* ((low 0)
	 (high (1- (fill-pointer ary))))
    (declare (type fixnum low high))
    
    (loop while (<= low high)
	  do (let* ((mid (ash (+ low high) -1))
		    (node (aref ary mid))
		    (node-key (trie-node-key node)))
	       (declare (type fixnum mid))
	       (cond ((funcall pred< node-key key)
		      (setf low (1+ mid)))
		     ((funcall pred< key node-key)
		      (setf high (1- mid)))
		     (t (return (values node mid)))))
	  finally (return (values nil (- (1+ low)))))))

(defun insert-child (ary pos key)
  (vector-push-extend nil ary)
  (loop for i from (1- (fill-pointer ary)) above pos
	do (setf (aref ary i) (aref ary (1- i))))
  (setf (aref ary pos) (make-trie-node :key key)))

(defun remove-empty (ary)
  (labels ((is-empty (node) (and (null (trie-node-value node))
				 (= 0 (fill-pointer (trie-node-children node))))))
    (let ((pos (position-if #'is-empty ary)))
      (if pos
	  (progn
	    (loop for n from pos below (1- (fill-pointer ary))
		  do (setf (aref ary n) (aref ary (1+ n))))
	    (setf (fill-pointer ary) (1- (fill-pointer ary)))
	    t)
	  nil))))

(defun trie-find-node (tr vec &optional (create nil))
  (loop with node = (trie-root tr)
	with modified = nil
	for key across vec
	do (let ((kids (trie-node-children node)))
	     (multiple-value-bind (found at) (binary-search kids key (trie-pred< tr))
	       (if found
		   (setf node found)
		   (if create
		       (setf node (insert-child kids (- (1+ at)) key) modified t)
		       (return (values nil modified))))))
	finally (return (values node modified))))

(defun trie-search (tr vec)
  (let ((node (trie-find-node tr vec)))
    (if node (trie-node-value node) nil)))

(defun (setf trie-search) (new-value tr vec)
  (if (null new-value)
      (error "new-value for trie cannot be nil"))

  (multiple-value-bind (node modified) (trie-find-node tr vec t)
    (if modified
	(incf (trie-size tr)))
    (setf (trie-node-value node) new-value)))

(defun trie-delete (tr vec)
  (let ((deleted nil))
    (labels ((delete-it (node vec)
	       (let ((found (binary-search (trie-node-children node) (aref vec 0) (trie-pred< tr))))
		 (if found
		     (if (= 1 (array-dimension vec 0))
			 (progn
			   (setf deleted (trie-node-value found) (trie-node-value found) nil)
			   (decf (trie-size tr))
			   deleted)
			 (if (delete-it found (make-array (1- (array-dimension vec 0))
							  :element-type (array-element-type vec)
							  :displaced-to vec :displaced-index-offset 1))
			     (remove-empty (trie-node-children found))
			     nil))
		     nil))))
      (if (delete-it (trie-root tr) vec)
	  (remove-empty (trie-node-children (trie-root tr)))))
    deleted))

;; todo: add iteration constructs, preferrably ones that could be used in standard lisp constructs like do, loop, or iterate
