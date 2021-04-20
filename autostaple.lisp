;; * Autostaple works as follows:
;; User Defined Params (:min-length int
;; 		     :max-length int
;; 		     :min-helix-length int
;; 		     :max-helix-length int
;; 		     :nucleation-dist int
;; 		     :?enforce-ends bool
;; 		     :cutoff-dist int
;; 		     :disallowed (list DNA)
;; 		     :breakpoints (list DNA-NT))
;; lat type: wireframe, surface, hex, square?

;; * 1: Get all partner nts (exculding :disallowed) withinin :cutoff-dist
;; Ensure antiparallel when matching, antiparallel is checked by dotproduct > *antiparallel-dotproduct*
;; Excepts scaffold, groups scaffold by elements?
;; * 2: Prune potential partners
;; ** Removing staple crossovers within 10 bps of a scaffold crossover and connecting the same two helices
;; Get staple crossover points, check if any pt-pairs are (within 10nts :5end and/or :3end) ?disallowed to?
;; If there are check if they are between the same helices that the scaffold crossover joins,
;; If there are any remove them and return list of pt pairs without them (non-destructive).
;; * Potentially later:
;; ** Try get matching end nts (nt2=next/nt1=prev or no relation)
;; ** If there are use them to start, else choose first partner nt pair
;; ** Go along by lattice type rules, hex closest to 21 bp, square:36, if not antiparallel error
;; *** print and check at this step*
;; ** Create long strands sucessively eliminating pt-pairs, if at the end of the strands capacity choose the next pt-pairs thats left and start again until all pt-pairs are done
;; *** print and check at this step*
;; ** break strands into parts between min and max length by some heuristic algo (not within 10 bps of a scaffold crossover and connecting the same two helices)
;; ** Return staples as list
;; * 3: Starting from scaffold 3' end
;; ** traverse by atleast min-helix-length then choose next pt-pair branch 
;; ** if we cross a disallowed nt, stop strand creation, return whole strand if >= min-length, else error
;; ** keep track of crossed pt-pairs
;; ** if (intersection crossed-pt-pairs all-pt-pairs) is the empty set continue
;; ** else repeat the process for next nt from 3' end of the scaffold that doesnt have a partner and isnt disallowed

;; Problems
;; How to make sure we dont span disallowed?
;; How to make sure every scaffold is visited?




;; Notes:

;; Avoid 3nt from target see Martin, T. G. & Dietz, H. Magnesium-free self-assembly of multi-layer DNA objects. Nature Communications 3, 1103 (2012)

;; appropriate helical positions are applied, except the staple
;; crossovers within 10 bps of a scaffold crossover and connecting the same two helices
;; (Supplementary Figure 14b).


(in-package :small)

(defclass/std dna-square-lattice (dna-origami)
  ((m :doc "The number of layers (layers are stacked in the z direction"
      :std 8)
   (n :doc "The number of helices (successive helices added in the x direction)"
      :std 8)
   (d :doc "The number of base paired helical turns (helices grow in the y direction)"
      :std 9)
   (scaff-loop-len-front
    :doc "The number of nucleotides before the main parallel helices start, in front of the 5 end of helix 0"
    :std 8)
   (scaff-loop-len-back
    :doc "The number of nucleotides before the main parallel helices start, in front of the 5 end of helix 0"
    :std 8))
  (:documentation "An implementation of Multilayer DNA Origami Packed on a Square Lattice (DOI: https://doi.org/10.1021/ja906381y). Note coordinate system differs from theirs, see the slot specs m, n and d for information of how these coordinates are defined."))

(defun helix-i-j (scaff-list i j)
  (find-obj-with-props
   (alexandria::flatten scaff-list) `((:i . ,i) (:j . ,j))))

(defun make-lattice-helix (i j)
    (let* ((num-nts (*  (/ 9 0.75) 8))
	   (vaxis (if (evenp (+ i j))
		      (v3 0 1 0)
		      (v3 0 -1 0)))
	   (vbb0 (if (evenp (+ i j)) ;; TODO check if this is correct
		     (v3 -1 0 0)
		     (v3 1 0 0)))
	   (x (* j (+ *helix-diameter* *helix-spacing*)))
	   (y (if (evenp (+ i j))
		  0
		  (* (- num-nts 1) ; subtract 1 so that helices are correctly aligned
		     *helix-nt-spacing*)))
	   (z (* -1.0 i (+ *helix-diameter* *helix-spacing*)))
	   (coords0 (v3 x y z))
	   (helix (helix-strand coords0 vaxis vbb0 num-nts)))
      (add-prop helix :i i)
      (add-prop helix :j j)
      helix))

(defun make-lattice-helices ()
    (loop for i from 0 to 7 
	  append (loop for j from 0 to 7
		       collect	(make-lattice-helix i j))))

(defmethod initialize-instance :after ((ori dna-square-lattice) &key)
  (let ((*rad/bp* (deg->rad 33.75)))
    (setf (scaffold ori) (make-lattice-helices))))



;; ================================ Auto Staple Functions ==================================
;; 1: Find Nearest neigbors



(defun nearest-partners (strand strands)
  "Returns a list of (strand-nt  [nearest nt in strands] dist-between-partnrs) for each strand-nt in strand"
  (let* ((nts-strand (strand-nts strand))
	 ;; Maybe remove non helices
	 (nts-all (alexandria:flatten (mapcar #'strand-nts strands))))
     (mapcar #'(lambda (nt-s)
		(mapcar #'(lambda (nt-a)
			    (list
			     nt-s
			     nt-a
			     (euclidean-distance (partner-coords nt-s)
						 (partner-coords nt-a))))
			nts-all))
	     nts-strand)))


(defun partner-dists (s1 s2 &optional threshold)
  "Returns a list of (strand-nt  [nearest nt in strands] dist-between-partnrs) for each strand-nt in strand"
  (let* ((nts1 (strand-nts s1))
	 ;; Maybe remove non helices
	 (nts2 (strand-nts s2))
	 (pt-dists (mapcar #'(lambda (nt-s1)
			       (mapcar #'(lambda (nt-s2)
					   (list
					    nt-s1
					    nt-s2
					    (euclidean-distance (partner-coords nt-s1)
								(partner-coords nt-s2))))
				       nts2))
			   nts1)))
    (mapcar #'sort-by-dist
	    (remove nil
		    (if threshold
			(mapcar #'(lambda (1nt-dists)
				    (remove-if #'(lambda (x)
						   (>= (third x)
						       threshold))				       
					       1nt-dists))
				pt-dists)
			pt-dists)))))
    

(defun sort-by-dist (dist-list)
  (sort (copy-list dist-list) #'< :key #'third))

;; (let (x)

;;   (push (partner-dists (first (scaffold l))
;; 	       (second (scaffold l))
;; 	       2)
;; 	x)
;;   (push (partner-dists (second (scaffold l))
;; 	       (third (scaffold l))
;; 	       2)
;; 	x))
  
(let* ((l (make-instance 'dna-square-lattice))
       (all-pts (partner-dists (first (scaffold l))
			       (second (scaffold l))
			       2))
       ;; (pts-as-nts (mapcar  #'(lambda (pts)	       
       ;; 				(mapcar #'(lambda (pt-pair)
       ;; 					    (list (make-partner (first pt-pair))
       ;; 						  (make-partner (second pt-pair))))
       ;; 					pts))
       ;; 			    all-pts))
       (pts-as-nts 
	 (mapcar #'(lambda (pt-pair)
		     (list (make-partner (first pt-pair))
			   (make-partner (second pt-pair))))
		 (mapcar #'first all-pts)))
       )
  (wmdna "tst"
	 (first (scaffold l))
	 (second (scaffold l))
	 pts-as-nts)
  (mapcar #'first all-pts))

			
		
    ;; (loop for nt in nts-strand collect
    ;; 			       (let* (nearest
    ;; 				      (a 1))
    ;; 				 (list nearest a)))

;; (progn
;;   (setf l (make-instance 'dna-square-lattice))
;;   (setf nns (nearest-partners (first (scaffold l)) (subseq (scaffold l) 1)))
;;   (setf fnn (sort (copy-list (first nns)) #'< :key #'third)))


;; (format t "~A~%~A" 
;; (first (second (partner-dists (first (scaffold l)) (second (scaffold l)) 2)))
;; (first (third (partner-dists (first (scaffold l)) (second (scaffold l)) 2))))



(defun possible-staple-partners (dists &optional (threshold 2.0))
  "Dists: (nt1 nt2 dist-between-partners), Returns all dists <= threshold"
  (remove-if #'(lambda (x)
		 (>= (third x) threshold))
	     dists))




(defun set-close-bases (dists &optional (base "X"))
  (mapcar #'(lambda (x)
	      (update-base (first dists)
			   (format nil "~A" base))
	      (update-base (second dists)
			   (format nil "~A" base)))
	  dists))

;; (set-close-bases (first (possible-staple-partners fnn 3)))

;; (let* ((l (make-instance 'dna-square-lattice))
;;        (pts (nearest-partners (first (scaffold l)) (subseq (scaffold l) 1)))
       
;;   pts)
  
       
;;   (mapcar #'set-close-bases
;; 	  (first (possible-staple-partners fnn 3)))
;;   (wmdna "NNp-lat-pts"
;;        (first (scaffold l))
;;        (second (scaffold l))
;;        (make-partner (first (scaffold l)))
;;        (make-partner (second (scaffold l)))))

			   
  




