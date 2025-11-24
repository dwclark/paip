(in-package :paip-trie)

(defstruct trie-node key value children)

(defstruct trie
  (pred< #'char<)
  (size 0)
  (root (make-trie-node)))

(defun binary-search (parent search-for pred<)
  (let* ((ary (trie-node-children parent))
	 (low 0)
	 (high (1- (if (null ary) 0 (array-dimension ary 0)))))
    (declare (type fixnum low high))
    
    (loop while (<= low high)
	  do (let* ((mid (ash (+ low high) -1))
		    (child (aref ary mid))
		    (key (trie-node-key child)))
	       (declare (type fixnum mid))
	       (cond ((funcall pred< key search-for)
		      (setf low (1+ mid)))
		     ((funcall pred< search-for key)
		      (setf high (1- mid)))
		     (t (return child))))
	  finally (return nil))))

(defun insert-child (parent key pred<)
  (let* ((ary (trie-node-children parent))
	 (child (make-trie-node :key key))
	 (new-size (1+ (if (null ary) 0 (array-dimension ary 0))))
	 (new-array (make-array new-size :element-type 'trie-node :initial-element child)))
    (if (< 1 new-size)
	(loop with index = 0
	      for element across ary
	      do (if (and key (funcall pred< key (trie-node-key element)))
		     (setf key nil
			   (aref new-array index) child
			   (aref new-array (1+ index)) element
			   index (+ 2 index))
		     (setf (aref new-array index) element
			   index (1+ index)))))
    (setf (trie-node-children parent) new-array)
    child))

(defun remove-empty (parent)
  (labels ((is-empty (node) (and (null (trie-node-value node))
				 (null (trie-node-children node)))))
    (let ((kids (trie-node-children parent)))
      (if (find-if #'is-empty kids)
	  (progn
	    (if (= 1 (array-dimension kids 0))
		(setf (trie-node-children parent) nil)
		(setf (trie-node-children parent) (remove-if #'is-empty kids)))
	    t)
	  nil))))

(defun trie-find-node (tr vec &optional (create nil))
  (loop with node = (trie-root tr)
	with modified = nil
	with pred< = (trie-pred< tr)
	for key across vec
	do (let* ((found (if (trie-node-children node)
			     (binary-search node key pred<)
			     nil)))
	     (if found
		 (setf node found)
		 (if create
		     (setf node (insert-child node key pred<) modified t)
		     (return (values nil modified)))))
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
	       (cond ((null node)
		      nil)
		     
		     ((zerop (array-dimension vec 0))
		      (setf deleted (trie-node-value node) (trie-node-value node) nil)
		      (decf (trie-size tr))
		      t)

		     ((delete-it (binary-search node (aref vec 0) (trie-pred< tr))
				 (make-array (1- (array-dimension vec 0))
					     :element-type (array-element-type vec)
					     :displaced-to vec :displaced-index-offset 1))
		      (remove-empty node)))))
      (delete-it (trie-root tr) vec))
    deleted))

;; todo: add iteration constructs, preferrably ones that could be used in standard lisp constructs like do, loop, or iterate
