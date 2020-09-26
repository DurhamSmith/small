(in-package :small)

(defclass/std dna-origami (dna)
  ((scaff :doc "The sub chem-objs defining the DNA origamis scaffold strand")))
;; )
;;   (:documentation "This class defines a DNA origami object. Its scaffold strand is defined as a list similar to subobjs. Its subobjs contain the other dna elements such as edge strands connectorn and staple strands/briges"))


(defgeneric add-staples (ori staples)
  "Adds a list of origami staples (DNA-STRAND) to oris subobjects. These subobjs have a property list on them which the property :scaffold=t")


