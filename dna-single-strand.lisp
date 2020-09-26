(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA single strand. Typically used for connecting strand. Not used to cerate helices, for that use DNA-HELIX-STRAND"))
