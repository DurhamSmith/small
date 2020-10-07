(defpackage :small-tests
  (:use #:cl #:parachute  #:small))

(in-package :small-tests)

(defun is-close (v1 v2 &key (e 1e-4))
  (is eq T (small::vec-close v1 v2 :e e)))

