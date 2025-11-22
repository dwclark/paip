(in-package :paip)

(defclass pattern-range () ())
(defgeneric range-matching-p (r))
(defgeneric in-range-p (r))
(defgeneric increment (r))
(defgeneric backfill-possible-p (r))

(defclass nil-range (pattern-range) ())
(defmethod range-matching-p ((r nil-range)) nil)
(defmethod in-range-p ((r nil-range)) nil)
(defmethod increment (r) (error "Illegal to increment a nil-range"))
(defmethod backfill-possible-p ((r nil-range)) nil)

(defclass base-range (pattern-range)
  ((so-far :initform 0 :accessor so-far)))

(defmethod increment ((r base-range))
  (incf (so-far r)))

(defclass min-max-range (pattern-range)
  ((min-matches :initarg :min-matches :accessor min-matches)
   (max-matches :initarg :max-matches :accessor max-matches)
   (so-far :initform 0 :accessor so-far)))

(defmethod in-range-p ((r min-max-range))
  (and (<= (min-matches r) (so-far r))
       (<= (so-far r) (max-matches r))))

(defmethod increment ((r min-max-range))
  (incf (so-far r)))

(defmethod backfill-possible-p ((r min-max-range))
  (and (zerop min-matches) (zerop so-far)))

(defparameter *the-nil-range* (make-instance 'nil-range))
