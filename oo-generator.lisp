;;Designed to be a replacement for paip chapter 2 code
;;I realize that chapter 2 is a stupid way of generating sometimes correct English sentences
;;and that I'm not actually improving the algorithm itself
(defgeneric random-child (p))
(defgeneric child-count (p))
(defgeneric add-producer (gramm prod))
(defgeneric get-producer (gramm the-key))

(defclass producer ()
  ((id :initarg :id :reader id)
   (grammar :initarg :grammar :accessor grammar)
   (children :initarg :children :reader children)))

(defmethod child-count ((p producer))
  (length (children p)))

(defmethod random-child ((p producer))
  (let ((idx (random (child-count p))))
    (elt (children p) idx)))

(defclass rule-producer (producer) ())

(defclass word-producer (producer) ())

(defclass grammar ()
  ((producers :initarg :producers :reader producers)))

(defmethod add-producer ((gramm grammar) (prod producer))
  (setf (gethash (id prod) (producers gramm)) prod))

(defmethod get-producer ((gramm grammar) the-key)
  (gethash the-key (producers gramm)))

(defun rules (name &rest lists)
  (let ((vectors (map 'vector (lambda (lst) (concatenate 'vector lst)) lists)))
    (make-instance 'rule-producer :id name :children vectors)))

(defun words (name strings)
  (make-instance 'word-producer
                 :id name
                 :children (concatenate 'vector strings)))

(defun make-grammar (&rest producers)
  (let ((g (make-instance 'grammar :producers (make-hash-table))))
    (dolist (producer producers)
      (setf (grammar producer) g)
      (add-producer g producer))
    g))

(defun generate-positions (gramm start-at)
  (let ((accum (make-array 16 :adjustable t :fill-pointer 0)))
    (labels ((inner (start-at)
               (let ((prod (get-producer gramm start-at)))
                 (if (eq 'word-producer (type-of prod))
                     (vector-push-extend prod accum)
                     (loop for x across (random-child prod) do (inner x))))))
      (inner start-at))
    accum))

(defun generate (gramm start-at)
  (format nil "~{~A~^ ~}" 
          (map 'list #'random-child (generate-positions gramm start-at))))

(defun generate-count (gramm start-at)
  (reduce #'* (generate-positions gramm start-at) :key #'child-count :initial-value 1))

(defun generate-all (gramm start-at)
  (let* ((positions (generate-positions gramm start-at))
         (indexes (make-array (length positions) :fill-pointer 0))
         (counter 0))
    (labels ((inner (my-pos)
               (loop for pos-idx from 0 below (child-count (elt positions my-pos))
                     do (progn
                          (vector-push pos-idx indexes)
                          (if (= my-pos (1- (length positions)))
                              (format t "~A) ~{~A~^ ~}~%"
                                      (incf counter)
                                      (loop for indexes-idx from 0 below (length indexes)
                                            collect (elt (children (elt positions indexes-idx)) (elt indexes indexes-idx))))
                              (inner (1+ my-pos)))
                          (vector-pop indexes)))))
      (inner 0))))

(defparameter *typed-grammar*
  (make-grammar (rules :sentence '(:noun-phrase :verb-phrase))
                (rules :noun-phrase '(:article :noun))
                (rules :verb-phrase '(:verb :noun-phrase))
                (words :article '("the" "a"))
                (words :noun '("man" "ball" "woman" "table"))
                (words :verb '("hit" "took" "saw" "liked"))))

(defparameter *bigger-grammar*
  (make-grammar (rules :sentence '(:noun-phrase :verb-phrase))
                (rules :noun-phrase '(:article :adj* :noun :pp*) '(:name) '(:pronoun))
                (rules :verb-phrase '(:verb :noun-phrase :pp*))
                (rules :pp* '() '(:pp :pp*))
                (rules :adj* '() '(:adj :adj*))
                (rules :pp  '(:prep :noun-phrase))
                (words :prep '("to" "in" "by" "with" "on"))
                (words :adj '("big" "little" "blue" "green" "adiabatic"))
                (words :article '("the" "a"))
                (words :name '("Pat" "Kim" "Lee" "Terry" "Robin"))
                (words :noun '("man" "ball" "woman" "table"))
                (words :verb '("hit" "took" "saw" "liked"))
                (words :pronoun '("he" "she" "it" "these" "those" "that"))))
