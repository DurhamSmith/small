(in-package :small)

(defun v3 (x y z &key (type-spec '(double-float)))
  "Make a 3D  vector with elements of type type-spec"
  (let* ((x (coerce x type-spec))
	 (y (coerce y type-spec))
	 (z (coerce z type-spec))
	 (v (from-list `(,x ,y ,z) '(3))))
    v))

;;; Accessor functions for 3D Vecs. We have the zero check so we can print 0 instead of -0 when we magicl:scale (todo create PR to fix this)

(defun v3-elem (v3 idx)
  "Returns v3[idx] and make sure 0 is always returned as 0 (not -0)"
  (let* ((elem (tref v3 idx))
	 (elem (if (zerop elem)
		   (coerce 0 (type-of elem))
		   elem)))
    elem))

(defun x (v3)
  (v3-elem v3 0))

(defun y (v3)
  (v3-elem v3 1))


(defun z (v3)
  (v3-elem v3 2))



(z (scale (v3 1 0 0) -1))

(defun print-v3 (v3 &key (stream nil) (prepend "") (append ""))
  "Prints V3 to stream."
  (format stream "~A~f ~f ~f~A" prepend (x v3) (y v3) (z v3) append))



(defun print-list (file lst) 
  "Prints each element of the lists in rest to file)"
  ;; TODO ADD TESTS
  (with-open-file (f file :direction :output :if-exists :supersede)
    (dolist (l lst)
      (format f "~A~%" l))))






