(defpackage :paip-tests
  (:use :cl :paip :suite-tests))

(in-package :paip-tests)

(defparameter *paip-internal-symbols*
  (list
   'paip::symbol-variable-p
   'paip::simple-matcher-p
   'paip::variable-matcher-p
   'paip::segment-matcher-p
   'paip::segment-variable
   'paip::variables-matched))

(dolist (s *paip-internal-symbols*) (import s))
