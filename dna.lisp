(in-package :small)



(defparameter *helix-spacing* 1d0 "Spacing between parallel double helices in this design")

(defparameter *helix-diameter* 2d0 "Diameter of DNA double helix in nm")

(defparameter *inter-helix-spacing* 1d0 "Spacing between two helices in an origmi design")

(defparameter *helix-nt-spacing* 0.34d0 "Length of each base pair in a double helix in nm")

(defparameter *single-strand-nt-spacing* 0.34d0 "Length of each base pair in a double helix in nm")

(defparameter *bp/turn* 10.44d0 "Length of each base pair in a double helix in nm")

(defparameter *rad/bp* (/ (* pi 2) *bp/turn*) "The delta theta between backbone vectors when viewed down the axis in the 5'->3' direction (- theta-nt1 theta-nt0)")



(defclass/std dna (chem-obj)
  ((prev :doc "The previous DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included")
   (next :doc "The next DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included"))
  (:documentation "A class for DNA chem-objs. Defines constants and connect methods"))

;(describe 'dna)


(defgeneric make-partner (obj)
  (:documentation "Return a complementary DNA CHEM-OBJ for the given DNA CHEM-OBJ")
  (:method ((obj dna))
    (error "generic function #'make-partner not implemented for ~A" (class-of obj))))


(defmethod dna-connect ((o1 dna) (o2 dna))
  "Returns VAUES o1 o2 after setting o2:prev = o1, o1:next = o2"
  (setf (next o1) o2)
  (setf (prev o2) o1)
  (values o1 o2))

(defgeneric nts (obj &key all)
  (:documentation "Gets DNA-NTs from dna CHEM-OBJ. If all = t all connected nts are returned if all all = nil then only the DNA-NTs in the strand are returned"))



(defgeneric next-nt (obj &key 5end kind)
  (:documentation "Returns a DNA-NT that would be the next nucleotide in the sequence for a given strand type")
  (:method (obj &key 5end kind)
    (error "(next-nt (obj ~A ) &key 5end) has not been implemented" (type-of obj))))


(defgeneric 5end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))

(defgeneric 3end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))




  
		  
		  
