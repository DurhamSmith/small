(in-package :small)


(defclass/std dna-triangle (dna-origami)
  ()
  (:documentation "An implementation the DNA a single triangle of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))


(defmethod initialize-instance :after ((ori dna-triangle) &key)  
  ;;Fist we loop over the scaffold so that we can set its sequence
  ;;This way when we make partners they have the correct seq
  (loop for i from 1 to 22 do
    (progn
      ;;(break "scaff ~A" (scaffold ori))
      (add-to-scaffold ori (scaffold-helix 1 i))
      (when (evenp i)
	(unless (= *2r* i)
	  (add-to-scaffold ori (SMALL::scaffold-loop 1 i)))
	)))
  (mapcar #'(lambda (nt base)
	    (with-accessors ((cm cm) (vbb vbb) (vn vn)) nt  
	      (update-base nt  base) ;Set the bases to match the m13 seq
	      ;; Update coords since we want regular carteisan
	      ;;and the paper defines y in the opposite direction
	      (setf (cm nt) (@ (from-diag '(1d0 -1d0 1d0)) cm)) 
	      (setf (vbb nt) (@ (from-diag '(1d0 -1d0 1d0)) vbb))
	      (setf (vn nt) (@ (from-diag '(1d0 -1d0 1d0)) vn))))
	(connected-nts (5nt (first (scaffold ori))))				
	(map 'list #'string  *m13mp18*))
  ori)


(defmethod write-oxdna ((obj dna-triangle) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (write-oxdna (5nt (first (scaffold obj))) :filename filename))

(write-oxdna (make-instance 'dna-triangle) :filename "tri")
