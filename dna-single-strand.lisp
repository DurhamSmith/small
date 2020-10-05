(in-package :small)

(defparameter *ss-cm-offset* 0d0 "The distance from the axis to the cm of a single-strand")

(defclass/std dna-single-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA single strand. Typically used for connecting strand. Not used to create helices, for that use DNA-HELIX-STRAND"))

(defmethod initialize-instance :after ((strand dna-single-strand) &key)
  "creates all dna-nt chem-objs for the strand"
  ;; loop for length and
  ;;; scale vaxis to get the dna-nts cm. use helixs vn and create nts with them (setting prev and next appropriately)
 
  )

(defun dna-ss (va vbb len &opt tfms seq)
  "returns a dna-helix-strand chem-obj with len nucleotides in its chem-objs class var"
  (make-instance 'dna-single-strand
		 :v-axis va
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))





;    (let* ((axis-coords (.+ cm (scale vbb -0.6))) ;vbb points from axis->backbone and vbb
(defun next-single-strand-nt-coords (cm vbb vn &key 5end)
  "returns coordinates for the next dna-nt extending in the 3' direction (or 5' if 5end = t) as
values cm vn vbb"
  (let* ((vbb+1 vbb) 
	 (cm+1 (.+ cm (scale vn
			     (if 5end ; choosing the direction we grow the strand
				 (- *single-strand-nt-spacing*)
				 *single-strand-nt-spacing*))))
	 (vn+1 (as-unit-vec vn)))
    (values cm+1 vbb+1 vn+1)))


(defun next-single-strand-nt (nt &key 5end)
  "returns the next dna-nt extending in the 3' direction (or 5' if 5end = t)"
  (with-accessors ((cm cm) (vbb vbb) (vn vn)) nt
    (unless (and cm vbb vn)
      (error "cm vbb and vn need to be set for n
t"))
    (multiple-value-bind (cm+1 vbb+1 vn+1)
	(next-single-strand-nt-coords cm vbb vn :5end 5end)
      (make-dna-nt :cm cm+1 :vbb vbb+1 :vn vn+1))))



(defun nucleotides-needed (v1 v2)
  (round
   (- (/ (euclidean-distance v1 v2)
	 *single-strand-nt-spacing*)
      1)))



(defun ss-from-nts (nts)
  "returns a dna-single-strand containing nts.
nts: string ordered from 5'->3'"
  ;;todo type/error checking
  (let* ((first-nt (first nts))
	 (last-nt (last nts))
	 (ss (make-instance 'dna-single-strand
			    :5nt first-nt
			    :3nt last-nt)))
    (connect-nts nts)
    (mapcar #'(lambda (nt)
		(add-child ss nt))
	    nts)
    ss))
	 

  
(defun bridging-single-strand (p1 p2 vbb &key 5end)
  "returns a values dna-single-strand strand length that bridges the distance with its 3 end starting at p1 + (midpoint p1 p2) - strand-len/2"
  (multiple-value-bind (num-nts extra-dist)
      (nucleotides-needed p1 p2)
    (let* ((vaxis (as-unit-vec (.- p2 p1)))
	   (vbb (as-unit-vec vbb))
	   (start-coord (.+ p1
			    (scale vaxis (/ extra-dist
					    2)))) ;; offsets since we might not have integer multiples of ss-nt-spacing
	   (cm (.+ start-coord
		    (scale vbb *ss-cm-offset*)))
	   (start-nt (make-dna-nt :cm cm
				  :vbb vbb
				  :vn vaxis))
	   (nts (append (list start-nt)
			(loop for x from 2 to num-nts collect
						      (next-single-strand-nt start-nt :5end 5end))))
	   (last-nt (last nts))
	   (ss (ss-from-nts nts)))
;      (break "~a ~a" nts ss)
      (values ss num-nts))))
	   
							  
      ;; (break "~A ~A" nts extra-dist)))


