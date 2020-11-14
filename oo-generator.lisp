;;Designed to be a replacement for paip chapter 2 code
;;I realize that chapter 2 is a stupid way of generating sometimes correct English sentences
;;and that I'm not actually improving the algorithm itself

;; definitions for producers
(defgeneric random-child (p))
(defgeneric child-count (p))
(defgeneric child-at (p idx))
(defgeneric add-producer (gramm prod))
(defgeneric get-producer (gramm the-key))

(defclass producer ()
  ((id :initarg :id :reader id)
   (grammar :initarg :grammar :reader grammar)
   (children :initarg :children :reader children)))

(defmethod child-count ((p producer))
  (length (children p)))

(defmethod child-at ((p producer) idx)
  (aref (children p) idx))

(defmethod random-child ((p producer))
  (child-at p (random (child-count p))))

(defclass rule-producer (producer) ())

(defclass word-producer (producer) ())

(defclass grammar ()
  ((producers :initarg :producers :reader producers)))

(defmethod add-producer ((gramm grammar) (prod producer))
  (setf (gethash (id prod) (producers gramm)) prod))

(defmethod get-producer ((gramm grammar) the-key)
  (gethash the-key (producers gramm)))

(defun word-producer-p (p)
  (eq 'word-producer (type-of p)))

;; generation functions (positions, random production, count, all)
(defun generate-positions (gramm start-at)
  (let ((accum (make-array 16 :adjustable t :fill-pointer 0)))
    (labels ((inner (start-at)
               (let ((prod (get-producer gramm start-at)))
                 (if (word-producer-p prod)
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
    (labels ((word-at (idx)
               (child-at (aref positions idx) (aref indexes idx)))
             
             (process-next ()
               (format t "~A) ~{~A~^ ~}~%"
                       (incf counter)
                       (loop for idx from 0 below (length indexes) collect (word-at idx))))
             
             
             (inner (my-pos)
               (loop for pos-idx from 0 below (child-count (aref positions my-pos))
                     do (progn
                          (vector-push pos-idx indexes)
                          (if (= my-pos (1- (length positions)))
                              (process-next)
                              (inner (1+ my-pos)))
                          (vector-pop indexes)))))
      
      (inner 0))))

;; convenience variables, functions, and macros for constructing a grammar
(defvar *current-grammar* nil)

(defun rule (id &rest lists)
  (let* ((vectors (map 'vector (lambda (lst) (concatenate 'vector lst)) lists))
         (me (make-instance 'rule-producer :id id :children vectors :grammar *current-grammar*)))
    (add-producer *current-grammar* me)))

(defun words (id strings)
  (add-producer *current-grammar*
                (make-instance 'word-producer
                               :id id
                               :children (concatenate 'vector strings)
                               :grammar *current-grammar*)))

(defmacro with-grammar (&body producers)
  `(let ((*current-grammar* (make-instance 'grammar :producers (make-hash-table))))
     ,@producers
     *current-grammar*))

;; defined grammars
(defparameter *typed-grammar*
  (with-grammar (rule :sentence '(:noun-phrase :verb-phrase))
                (rule :noun-phrase '(:article :noun))
                (rule :verb-phrase '(:verb :noun-phrase))
                (words :article '("the" "a"))
                (words :noun '("man" "ball" "woman" "table"))
                (words :verb '("hit" "took" "saw" "liked"))))

(defparameter *bigger-grammar*
  (with-grammar (rule :sentence '(:noun-phrase :verb-phrase))
                (rule :noun-phrase '(:article :adj* :noun :pp*) '(:name) '(:pronoun))
                (rule :verb-phrase '(:verb :noun-phrase :pp*))
                (rule :pp* '() '(:pp :pp*))
                (rule :adj* '() '(:adj :adj*))
                (rule :pp  '(:prep :noun-phrase))
                (words :prep '("to" "in" "by" "with" "on"))
                (words :adj '("big" "little" "blue" "green" "adiabatic"))
                (words :article '("the" "a"))
                (words :name '("Pat" "Kim" "Lee" "Terry" "Robin"))
                (words :noun '("man" "ball" "woman" "table"))
                (words :verb '("hit" "took" "saw" "liked"))
                (words :pronoun '("he" "she" "it" "these" "those" "that"))))
