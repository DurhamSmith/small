(defpackage :small-tests
  (:use #:cl #:parachute  #:small))

(in-package :small-tests)

(defun is-close (v1 v2 &key (e 1e-4))
  (is eq T (small::vec-close v1 v2 :e e)))

(defun DI (data k i j &key (v? t))
  "Data index"
  (let ((data (nth (- j 1) (nth (- i 1) (nth (- k 1) data)))))
    (if v?
	(v3l data)
	data)))

