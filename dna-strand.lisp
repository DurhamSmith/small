(in-package :small)
;;;; NOTE: strand nt subobjs should can contain ids nt number in that strands sequence, them the strand can be gotten (traverse length) or the strand and its partner can be gotten (traverse prev next) or check if a strand is a solo strand or part of a  composite one (len = prev-next traversal len

(defclass/std dna-strand (dna)
  ((vaxis :doc "A vector pointing in the 5'->3' direction")
   (vbb :doc "A vector pointing in from the base towards the backbone")
   (vstart :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (vend :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand")
   (seq :doc "The sequence of the strand (length must be equal to len)"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

(defmethod connect ((o1 dna-strand) (o2 dna-strand) &rest rest)
  "Sets (end o1) = (end o2) and connects their nucleotides"  
  (dna-connect o1 o2))

(defun make-dna-strand (&rest rest)
  (make-instance 'dna-strand))



 
