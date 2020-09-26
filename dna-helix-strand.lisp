(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA double helix strand"))





(defun make-helix-strand (va vbb len &opt tfms)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDE subobjs"
  (make-instance 'dna-helix-strand :v-axis va :v-bb vbb :len len :tfms tfms))
