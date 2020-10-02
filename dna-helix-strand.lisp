(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA double helix strand"))

(defparameter *helix-cm-offset* 0.6d0 "Distance from helix axis to NT center of mass")

(defmethod initialize-instance :after ((strand dna-helix-strand) &key)
  "Creates all DNA-NT CHEM-OBJS for the strand"
  ;; Loop for length and
  ;;; Rotate vbb and scale vaxis to get nts vbb and cm. use helixs vn and create nts with them (setting prev and next appropriately)
  )


(defun helix-axis (cm vbb)
  (.- cm (scale vbb *helix-cm-offset*)))
;(helix-axis (v3 1.6 1 0) (v3 1 0 0))

(defun next-helix-vbb (vbb vn &key 5end)
    (if 5end
	(as-unit-vec (rotate-vec vbb vn (- *rad/bp*)))
	(as-unit-vec (rotate-vec vbb vn *rad/bp*))))

(defun next-helix-cm (cm vbb vn &key 5end)
  (let* ((ax (helix-axis cm vbb))
	 (ax+1 (.+ ax (scale vn
			     (if 5end ; Choosing the direction we grow the axis
				 (- *helix-nt-spacing*)
				 *helix-nt-spacing*))))
	 (vbb+1 (next-helix-vbb vbb vn :5end 5end))
	 (cm+1 (.+ ax+1 vbb+1)))
    cm+1))
	
;    (let* ((axis-coords (.+ cm (scale vbb -0.6))) ;vbb points from axis->backbone and vbb
(defun next-helix-nt-coords (cm vbb vn &key 5end)
  "Returns coordinates for the next DNA-NT extending in the 3' direction (or 5' if 5end = t) as
VALUES cm vn vbb"
  (let* ((vbb+1 (next-helix-vbb vbb vn :5end 5end))
	 (cm+1 (next-helix-cm cm vbb vn :5end 5end))
	 (vn+1 (as-unit-vec vn)))
    (values cm+1 vbb+1 vn+1)))





(defun make-helix-strand (vax vbb len &opt tfms seq)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDEs in its chem-objs class var"
  (make-instance 'dna-helix-strand
		 :v-axis vax
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))
