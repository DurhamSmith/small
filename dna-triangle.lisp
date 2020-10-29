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
  ;(break ori)
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

  (setf (5nt ori) (5nt (find-obj-with-props (scaffold ori)
					       `((:i . 1) (:k . 1)))))
  (setf	(3nt ori) (3nt (find-obj-with-props (scaffold ori)
					       `((:i . 22) (:k . 1)))))
  ori)


(defmethod write-oxdna ((obj dna-triangle) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (write-oxdna (5nt (first (scaffold obj))) :filename filename))

(write-oxdna (make-instance 'dna-triangle) :filename "tri")

(defmethod connect ((o1 dna-triangle) (o2 dna-triangle) &rest rest)
  (dna-connect o1 o2)
  (connect (3nt o1) (5nt o2)))

(defun tri-edge (tri &key from22)
  "Returns a unit vector along the triangles edge
if from22=t then the vector will point from helix 22->21"
  (let ((nt1 (3nt (first (scaffold tri))))
	(nt2 (5nt (car (last (scaffold tri))))))
    (as-unit-vec (if from22
		     (nt1->nt2 nt2 nt1)
		     (nt1->nt2 nt1 nt2)))))

(defun edge->center (tri &key reverse)
  (let ((nt1 (3nt (first (scaffold tri))))
	(nt2 (5nt (first (scaffold tri)))))
    (as-unit-vec (if reverse
		     (nt1->nt2 nt2 nt1)
		     (nt1->nt2 nt1 nt2)))))




