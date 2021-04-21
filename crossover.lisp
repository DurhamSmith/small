(in-package :small)

(defclass/std crossover ()
  ((nt1 :doc "The nt whichs partner will be on the 5' end of the crossover")
   (nt2 :doc "The nt whichs partner will be on the 3' end of the crossover")
   (pt1 :doc "5' of crossover")
   (pt2 :doc "3' of crossover")
   (dist :doc "The length in nm between pt1 and pt2")))


(defmethod initialize-instance  ((c crossover) &key)
  (with-slots ((nt1 nt1) (nt2 nt2) (dist dist)) c    
    (setf dist (euclidean-distance (partner-coords nt1)
				   (partner-coords nt2))))
  c)

(defun make-crossovers (s1 s2)
  (let ((nts1 (strand-nts s1))
	(nts2 (strand-nts s2)))
    (mapcar #'(lambda (nt-s1)
		(mapcar #'(lambda (nt-s2)
			    (make-instance 'crossover
					   :nt1 nt-s1
					   :nt2 nt-s2))
			nts2))
	    nts1)))
    
   
