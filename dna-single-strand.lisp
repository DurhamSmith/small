(in-package :small)



(defclass/std dna-single-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA single strand. Typically used for connecting strand. Not used to create helices, for that use DNA-HELIX-STRAND"))

(defmethod initialize-instance :after ((strand dna-single-strand) &key)
  "Creates all DNA-NT CHEM-OBJs for the strand"
  ;; Loop for length and
  ;;; scale vaxis to get the DNA-NTs cm. use helixs vn and create nts with them (setting prev and next appropriately)
 
  )

(defun make-single-strand (va vbb len &opt tfms seq)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDEs in its chem-objs class var"
  (make-instance 'dna-single-strand
		 :v-axis va
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))



;    (let* ((axis-coords (.+ cm (scale vbb -0.6))) ;vbb points from axis->backbone and vbb
(defun next-single-strand-nt-coords (cm vbb vn &key 5end)
  "Returns coordinates for the next DNA-NT extending in the 3' direction (or 5' if 5end = t) as
VALUES cm vn vbb"
  (let* ((vbb+1 vbb) 
	 (cm+1 (.+ cm (scale vn
			     (if 5end ; Choosing the direction we grow the strand
				 (- *single-strand-nt-spacing*)
				 *single-strand-nt-spacing*))))
	 (vn+1 (as-unit-vec vn)))
    (values cm+1 vbb+1 vn+1)))


(defun next-single-strand-nt (nt &key 5end)
  "Returns the next DNA-NT extending in the 3' direction (or 5' if 5end = t)"
  (with-accessors ((cm cm) (vbb vbb) (vn vn)) nt
    (unless (and cm vbb vn)
      (error "cm vbb and vn need to be set for nt"))
    (multiple-value-bind (cm+1 vbb+1 vn+1)
	(next-single-strand-nt-coords cm vbb vn :5end 5end)
      (make-dna-nt :cm cm+1 :vbb vbb+1 :vn vn+1))))


(defun bridging-single-strand (p1 p2 &key 5end)
  "Returns a VALUES DNA-SINGLE-STRAND strand length that bridges the distance with its 3 end starting at p1 + (midpoint p1 p2) - strand-len/2"
)
