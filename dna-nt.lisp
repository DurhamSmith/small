(in-package :small)
;;;; This a base class for DNA CHEM-OBJs.
(defclass/std dna-nt (dna)
  ((cm :doc "Vector of the nucleotides center of mass coordinates")
   (vbb :doc "Unit vector pointing from base->backbone")
   (vn :doc "Unit vector normal to the face of the DNA base in the 5'->3' direction")
   (v :doc "The velocity"
      :std (v3 0 0 0))
   (L :doc "The angular velocity"
      :std (v3 0 0 0)))
  (:documentation "A class for a DNA nucleotide CHEM-OBJ. NTs are defined similary to that of oxdna using a center of mass, a vector from base to backbone and a vector normal to the face of the base. Our vn and vbb are defined OPPOSITE to that of oxdna"))


;;;; Generic Functions specific to DNA CHEM-OBJs (only exist when specilized on DNA CHEM-OBJs



;;;; Generic Functions specialized on DNA CHEM-OBJs


;;;; Creation Functions
(defun make-dna-nt (&key cm vbb vn tfms)
  "Returns a DNA-NT CHEM-OBJ with the correctly initialized slots"
  (make-instance 'dna-nt :cm cm :vbb vbb :vn vn :tfms tfms ))

(defgeneric oxdna-config (obj &key &allow-other-keys)
  (:documentation "Returns the oxdna configuration of the object as a (TODO datatype). DNA/RNA NUCLEOTIDEs will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config")
  (:method ((obj dna-nt) &key &allow-other-keys)
    (with-accessors ((cm cm) (vbb vbb) (vn vn) (v v) (L L)) obj
      (concatenate 'string 
		   (print-v3 cm)
		   (print-v3 vbb :prepend " ")
		   (print-v3 vn :prepend " ")
		   (print-v3 v :prepend " ")
		   (print-v3 L :prepend " ")))))



