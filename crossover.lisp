(in-package :small)

(defclass/std crossover ()
  ((nt1 :doc "The nt whichs partner will be on the 5' end of the crossover")
   (nt2 :doc "The nt whichs partner will be on the 3' end of the crossover")
   (pt1 :doc "5' of crossover")
   (pt2 :doc "3' of crossover")
   (dist :doc "The length in nm between pt1 and pt2")))


(defmethod initialize-instance :after  ((c crossover) &key)
  (with-slots ((nt1 nt1) (nt2 nt2) (dist dist)) c    
    (setf dist (euclidean-distance (partner-coords nt1)
				   (partner-coords nt2))))
  c)

(defun make-crossovers (s1 s2 &key (cutoff-dist *cutoff-dist*))
  "Returns all crossovers between nts in DNA-HELIX-STRAND s1 and DNA-HELIX-STRAND s2 within cutoff-dist"
  (let ((nts1 (strand-nts s1))
	(nts2 (strand-nts s2))
	crossover
	all-crossovers)
    (setf all-crossovers
	  (mapcar #'(lambda (nt-s1)
		      (mapcar #'(lambda (nt-s2)
				  (setf crossover
					(make-instance 'crossover
						       :nt1 nt-s1
						       :nt2 nt-s2))
				  (if cutoff-dist
				      (when (below-cutoffp crossover :cutoff-dist cutoff-dist)
					crossover)
				      crossover))
			      nts2))	    
		  nts1))
    (remove-nilr all-crossovers)))
    
   


(defun remove-nilr (x)
  (if (consp x)
      (mapcar #'remove-nilr (remove nil x))
      x))


  


    
    

(defun below-cutoffp (crossover &key (cutoff-dist *cutoff-dist*))
  (<= (dist crossover) cutoff-dist))
