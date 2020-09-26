(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA double helix strand"))



(defmethod initialize-instance :after ((strand dna-helix-strand) &key)
  "Creates all DNA-NT CHEM-OBJS for the strand"
  ;; Loop for length and
  ;;; Rotate vbb and scale vaxis to get nts vbb and cm. use helixs vn and create nts with them (setting prev and next appropriately)
  )

(defun make-helix-strand (vax vbb len &opt tfms seq)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDEs in its chem-objs class var"
  (make-instance 'dna-helix-strand
		 :v-axis vax
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))
