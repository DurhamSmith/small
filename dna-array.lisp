(in-package :small)
(progn
  (defun dna-array-from-list (lst &key (cube-type 'dna-cube))
    (let* ((box-size 70)
	   (x (v3 0 0 0))
	   (y (v3 0 0 0))
	   (y (v3 0 0 0))
	   (z (v3 0 0 0))
	   (inc-x (v3 box-size 0 0))
	   (inc-y (v3 0 box-size 0))
	   (inc-z (v3 0 0 box-size))
	   (cube-array     
	     (mapcar #'(lambda (zframe)
			 (setf z (.+ z inc-z))
			 (setf x (v3 0 0 0))
			 (mapcar #'(lambda (xrow)
				     (setf x (.+ x inc-x))
				     (setf y (v3 0 0 0))
				     (mapcar #'(lambda (has-cube)
						 (setf y (.+ y inc-y))
						 (let ((cube (if has-cube
								 (make-instance 'dna-cube)
								 nil)))
						   (when cube
						     (format t "~A~%" (.+ x
									  (.+ y z)))
						     (translate-obj cube
								    (.+ x
									(.+ y z))))))
					     xrow))
				 zframe))
			 lst)))
      cube-array))




(time
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
    (list nil nil nil nil)))))

(time (defparameter *arr* (dna-array-from-list *sq*)))
)(break "~A" *arr*)
(setf *arr* t)

(sb-profile:profile "SMALL")
(sb-profile:report)
(sb-profile:reset)
(stap-bridges (make-instance 'dna-cone))
(time
 (wmdna "shuttleworth"
	(alexandria:flatten (mapcar #'all-to-write (alexandria:flatten *arr*)))))
 )
(setq j 1)
(1+ j)
(format nil "~A" 1)


(time
 (let ((i 1))
  (mapcar #'(lambda (cube)
	      (setf i (1+ i))
	      (wmdna (format nil "~A" i)
		     (all-to-write cube)))
	  (alexandria:flatten *arr*))))



(car (alexandria:flatten *arr*))
(dolist (x (alexandria:flatten (mapcar #'all-to-write (alexandria:flatten *arr*))))
  (format t "~A~%" (length (all-tfms x))))


(mapcar #'(lambda (x)
	    (format t "~A~%" (parent x)))
	(flatten (all-to-write (car (alexandria:flatten *arr*)))))

(count nil (mapcar #'(lambda (x)
	    (format t "~A~%" (parent x)))
	(flatten (all-to-write (car (alexandria:flatten *arr*))))))

(filter #'(lambda (x)
		    
		      x))
		(alexandria:flatten (mapcar #'all-to-write (alexandria:flatten *arr*))))


(time
 (wmdna "orc7"
	(filter #'(lambda (x)
		    (when (typep x 'dna-nt)
		      x))
		(alexandria:flatten (mapcar #'all-to-write (alexandria:flatten *arr*))))))


(time
 (wmdna "orc3"
        ;; (all-to-write
	;;  (second (second (second *arr*)))
	;;  )
	(filter #'(lambda (x)
		    (when (typep x 'dna-nt)
		      x))
		(alexandria:flatten
		 (all-to-write
		  (second (second (second *arr*))))))
	(filter #'(lambda (x)
		    (when (typep x 'dna-nt)
		      x))
		(alexandria:flatten
		 (all-to-write
		  (second (second (third *arr*))))))
	))

(all-tfms (second (second (third *arr*))))
(tfms (third (second (second *arr*))))
(wmdna "owl"
       (all-to-write (second (second (third *arr*)))))




						       ;; (wmdna (list (second (second (second *arr*)))
	     ;; 	   (second (second (third *arr*))))
	     ;; :filename "bro")



(wmdna "so" (first
	      (alexandria:flatten
	      (mapcar #'all-to-write
		     (remove nil (alexandria:flatten
				  *arr*))))))


(wmdna "f" (first
	      (alexandria:flatten
	      (mapcar #'all-to-write
		     (remove nil (alexandria:flatten
				  *arr*))))))

(time (wmdna "arr" (mapcar #'all-to-write
		     (remove nil (alexandria:flatten
				  *arr*)))))




(let* ((t1 (SMALL::make-dna-tile))
       (stap (SMALL::s-staple t1 1 2 '(23 16 16) '(8 15 7))))
  (SMALL::wmdna "a-s" (append
		       (list (first (SMALL::scaffold t1)))
		       (small::internal-staples t1)))
  (small::internal-staples t1))





;;; TODO EDGE STAPLS AND BRIDGE STAPS
;; OXDNA Model
