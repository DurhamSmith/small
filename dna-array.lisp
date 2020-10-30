(in-package :small)

(defun dna-array-from-list (lst &key (cube-type 'dna-cube))
  (let* ((box-size 70)
	 (x (v3 0 0 0))
	 (y (v3 0 0 0))
	 (z (v3 0 0 0))
	 (inc-x (v3 box-size 0 0))
	 (inc-y (v3 0 box-size 0))
	 (inc-z (v3 0 0 box-size))
	 (cube-array     
	   (mapcar #'(lambda (zframe)
		       (let ((z (.+ z inc-z)))
			 (mapcar #'(lambda (xrow)
				     (let ((x (.+ x inc-x)))
				       (mapcar #'(lambda (has-cube)
						   (let ((y (.+ y inc-y))
							 (cube (if has-cube
								   (make-instance 'dna-cube)
								   nil)))
						     (when cube
						       (translate-obj cube
								      (.+ x
									  (.+ y z))))))
					       xrow)))
				 zframe)))
		   lst)))
    cube-array))



(defparameter arr (dna-array-from-list *sq*))

(defparameter *sq*
  (list
   (list
    (list nil nil nil nil)
    (list nil nil nil nil)
    (list nil nil nil nil)
    (list nil nil nil nil))
   (list
    (list nil nil nil nil)
    (list nil t   t   nil)
    (list nil t   t   nil)
    (list nil nil nil nil))
   (list
    (list nil nil nil nil)
    (list nil t   t   nil)
    (list nil t   t   nil)
    (list nil nil nil nil))
   (list
    (list nil nil nil nil)
    (list nil nil nil nil)
    (list nil nil nil nil)
    (list nil nil nil nil))))
