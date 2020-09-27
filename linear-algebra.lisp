(in-package :small)

(defun v3 (x y z &key (type-spec '(double-float)))
  "Make a 3D  vector with elements of type type-spec"
  (let* ((x (coerce x type-spec))
	 (y (coerce y type-spec))
	 (z (coerce z type-spec))
	 (v (from-list `(,x ,y ,z) '(3))))
    v))


(defun x (v3)
  (tref v3 0))

(defun y (v3)
  (tref v3 1))

(defun z (v3)
  (tref v3 2))


(defun print-v3 (v3 &key (stream t))
  "Prints V3 to stream"
  (format stream "~f ~f ~f" (x v3) (y v3) (z v3)))



(shape (v3 0 0 0))



