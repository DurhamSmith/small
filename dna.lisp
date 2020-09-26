(in-package :small)

(defclass/std dna (chem-obj)
  ((prev :doc "The previous DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included")
   (next :doc "The next DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included"))
  (:documentation "A class for DNA chem-objs. Defines constants and connect methods"))

(describe 'dna)


(defgeneric make-partner (obj)
  (:documentation "Return a complementary DNA CHEM-OBJ for the given DNA CHEM-OBJ")
  (:method ((obj dna))
    (error "generic function #'make-partner not implemented for ~A" (class-of obj))))
