(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA single strand. Typically used for connecting strand. Not used to create helices, for that use DNA-HELIX-STRAND"))

(defmethod initialize-instance :after ((strand dna-single-strand) &key)
  "Creates all DNA-NT CHEM-OBJs for the strand"
  ;; Loop for length and
  ;;; scale vaxis to get the DNA-NTs cm. use helixs vn and create nts with them (setting prev and next appropriately)
 
  )

(defun make-single-strand (va vbb len &opt tfms seq)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDEs in its chem-objs class var"
  (make-instance 'dna-helix-strand
		 :v-axis va
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))

