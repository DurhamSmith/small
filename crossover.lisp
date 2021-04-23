(in-package :small)



;; ======================================Class Functions========================================
(defclass/std crossover ()
  ((nt1 :doc "The nt whichs partner will be on the 5' end of the crossover")
   (nt2 :doc "The nt whichs partner will be on the 3' end of the crossover")
   (pt1 :doc "5' of crossover")
   (pt2 :doc "3' of crossover")
   (dist :doc "The length in nm between pt1 and pt2")
   (bbdot :doc "dotproduct between vbbs")
   (bbangle :doc "angle between vbbs")
   (planar :doc "t or false if vbb of nt1 and nt2 are in the same plane")))


(defmethod initialize-instance :after  ((c crossover) &key)
  (with-slots ((nt1 nt1) (nt2 nt2) (dist dist) (bbdot bbdot) (bbangle bbangle)) c
    (multiple-value-bind (pcm1 pvbb1) (partner-coords nt1)
      (multiple-value-bind (pcm2 pvbb2) (partner-coords nt2)	
	(setf dist (euclidean-distance pcm1 pcm2)
	      bbdot (dotproduct pvbb1 pvbb2)
	      bbangle (acos bbdot))
	c))))

(defmethod nts ((c crossover) &key all)
  (list (nt1 c) (nt2 c)))
    
;; ==========================Helper functions =====================================





(defun conflictingp (crossover1 crossover2)
  "Predicate to test if two crossovers share nucleotides. 
Returns a list of all conflicting nts in both crossovers or nil"
  (let ((conflicting-nts (intersection (nts crossover1)
				       (nts crossover2))))
    (values conflicting-nts
	    (and conflicting-nts (list crossover1 crossover2)))))






(defun all-conflicting-crossovers (crossovers)
  "crossovers: (list CROSSOVER ...)
Returns
(list (list CROSSOVER ...)) where each of the nested lists conflict with one another
;TODO: Check that reduce, union is what we want"
  (remove-all-nils
   (if (cdr crossovers)
       (cons (reduce #'union (mapcar #'(lambda (c)
			 (multiple-value-bind (nts xovers)
			     (conflictingp (car crossovers) c)
			   xovers
			  ) 
			)
		    (cdr crossovers)))
	    (all-conflicting-crossovers (cdr crossovers)))
       nil)))

(defun resolve-conflicting-crossovers (all-crossovers conflicting-crossovers)
  "all-crossovers: (list CROSSOVER)
conflicting-crossovers: (list (list CROSSOVER ...))
Selects the worst crossovers per nested list in conflicting-crossovers and removes them"
  (remove-crossovers all-crossovers
		     (mapcar #'worst-crossovers
			     (all-conflicting-crossovers all-crossovers))))


(defun remove-crossovers (all remove)
  "all : (list CROSSOVER)
remove : (list CROSSOVER)
Lists can be nested, returned list is flat
;TODO: Should returned list be the same structure just with crossovers removed?"
  (set-difference (flatten all) (flatten remove)))



(defun strand-crossovers (s1 s2 &key (cutoff-dist *cutoff-dist*))
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
    (remove-all-nils all-crossovers)))



(defun make-crossovers (strand partner-strands)
  "strand : DNA-HELIX-STRAND
partner-strands: (list DNA-HELIX-STRAND)
Returns: (list [CROSSOVER | (list CROSSOVER ...)])"
  (reduce #'append
	(mapcar  #'(lambda (pt)
		(strand-crossovers strand
				   pt))
     partner-strands)))

    
(defun bbangled (crossover)
  "returns bbangle in degrees"
  (rad->deg (bbangle crossover)))



(defun find-best-crossover (crossovers)
  "Current implementation returns multiples valus, 
first: best crossover (by shortest distance)
second all crossovers sorted by distance
Old Idea: 
Takes a list of CROSSOVERS and finds the best one of them by checking
1: distance 
2: planarp
3: angle"
  (cond ((consp crossovers)
	 (let ((ordered (sort (copy-list crossovers) #'< :key #'dist)))
	   (values (car ordered) ordered)) )
	((typep crossovers 'crossover) crossover)
	(t (error "crossovers of wrong type"))))


  (typep (make-instance 'dna-nt) 'dna-nt)
  (if (consp crossovers)
  )

(defun worst-crossovers (crossovers)
  (multiple-value-bind (best all)
      (find-best-crossover crossovers)
    (nreverse (remove best all))))
    




(defun planarp (crossover)
  (let ((cm1 (vbb (nt1 crossover)))
	(cm2 (vbb (nt2 crossover)))
    (MAGICL:.- cm1 cm2))))



(defun remove-all-nils (x)
  "Recursively removes all nils from nested lists"
  (remove nil (remove-nilr x)))
		    
	    

(defun remove-nilr (x)
  "Todo use LABELs and move to remove-all-nils"
	  (if (consp x)
	      (remove nil
		      (mapcar #'remove-nilr (remove nil x)))
	      x))


     

(defun below-cutoffp (crossover &key (cutoff-dist *cutoff-dist*))
  (<= (dist crossover) cutoff-dist))
