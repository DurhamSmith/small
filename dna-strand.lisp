(in-package :small)

(defclass/std dna-strand (dna)
  ((vaxis :doc "A vector pointing in the 5'->3' direction")
   (vbb :doc "A vector pointing in from the base towards the backbone")
   (vstart :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (vend :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand")
   (seq :doc "The sequence of the strand (length must be equal to len)"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

