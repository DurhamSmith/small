(in-package :small)

(defclass/std dna-strand (dna)
  ((v-axis :doc "A vector pointing in the 5'->3' direction")
   (v-bb :doc "A vector pointing in from the base towards the backbone")
   (v-start :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (v-end :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

