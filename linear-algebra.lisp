(in-package :small)

(defun v3 (x y z &key (type-spec '(double-float)))
  "Make a 3D  vector with elements of type type-spec"
  (let* ((x (coerce x type-spec))
	 (y (coerce y type-spec))
	 (z (coerce z type-spec))
	 (v (from-list `(,x ,y ,z) '(3))))
    v))

(defun v3l (l  &key (type-spec '(double-float)))
  (v3 (nth 0 l) (nth 1 l) (nth 2 l)))

(defun as-unit-vec (v)
  (scale v (/ 1 (norm v))))
    

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

(defun bounds (&rest vecs)
  "Returns VALUES with the first being a v3 with x-coords 
= max-x-of-all-vecs - min-x-of-all-vecs 
 y and z entires are computed similarly
and the second two the vectors containing the min and max coords respectively "
  (let* ((vecs (if (typep (car vecs) 'cons)
		   (first vecs)  ;to handle vecs = (v1 ..) & ((v1 ...))
		   vecs))
	 (mx (on-v3-axis #'min 'x vecs :by #'reduce))
	 (my (on-v3-axis #'min 'y vecs :by #'reduce))
	 (mz (on-v3-axis #'min 'z vecs :by #'reduce))
	 (min (v3 mx my mz))
	 (mx (on-v3-axis #'max 'x vecs :by #'reduce))
	 (my (on-v3-axis #'max 'y vecs :by #'reduce))
	 (mz (on-v3-axis #'max 'z vecs :by #'reduce))
	 (max (v3 mx my mz))
	 (range (.- max min)))
    (values range min max)))
	 

(defun on-v3-axis (fn axis vecs &key (by #'mapcar))
  "Applies fn to a LIST by by of all elements of (axis vec) for vec in vecs
fn: FUNCTION
axis: SYMBOL ('x 'y 'z are vaild inputs)
vecs: LIST
by: The list mapping function
"
  ;;TODO handle vecs as a list or as rest params
  (let* ((all-elems (mapcar axis vecs)))
    (funcall by fn all-elems)))
	      


(z (scale (v3 1 0 0) -1))

(defun print-v3 (v3 &key (stream nil) (prepend "") (append ""))
  "Prints V3 to stream."
  (format stream "~A~f ~f ~f~A" prepend (x v3) (y v3) (z v3) append))



(defun write-list (file lst) 
  "Prints each element of the lists in rest to file)"
  ;; TODO ADD TESTS
  (with-open-file (f file :direction :output :if-exists :supersede)
    (dolist (l lst)
      (format f "~A~%" l))))






