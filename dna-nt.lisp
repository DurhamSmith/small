(in-package :small)
;;;; This a base class for DNA CHEM-OBJs.
(defclass/std dna-nt (dna)
  ((cm :doc "Vector of the nucleotides center of mass coordinates")
   (vbb :doc "Unit vector pointing from base->backbone")
   (vn :doc "Unit vector normal to the face of the DNA base in the 5'->3' direction"))
  (:documentation "A class for a DNA nucleotide CHEM-OBJ. NTs are defined similary to that of oxdna using a center of mass, a vector from base to backbone and a vector normal to the face of the base. Our vn and vbb are defined OPPOSITE to that of oxdna"))


;;;; Generic Functions specific to DNA CHEM-OBJs (only exist when specilized on DNA CHEM-OBJs



;;;; Generic Functions specialized on DNA CHEM-OBJs


;;;; Creation Functions
(defun make-dna-nt (&key cm vbb vn tfms)
  "Returns a DNA-NT CHEM-OBJ with the correctly initialized slots"
  (make-instance 'dna-nt))
  
