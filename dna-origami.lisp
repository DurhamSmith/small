(in-package :small)



(defclass/std dna-staple-strand (dna-strand)
 ())


(defun staple-partner (scaff-obj)
  (typecase scaff-obj
    (DNA (partner

(defun create-staple (&rest scaff-objs)
  "Creates a partner for each scaff-obj"
  (mapcar #'staple-partner scaff-objs)
  



(defclass/std dna-origami (dna)
  ((scaffold :doc "The sub chem-objs defining the DNA origamis scaffold strand")))
;; )
;;   (:documentation "This class defines a DNA origami object. Its scaffold strand is defined as a list similar to subobjs. Its subobjs contain the other dna elements such as edge strands connectorn and staple strands/briges"))


;; (defgeneric add-staples (ori staples)
;;   "Adds a list of origami staples (DNA-STRAND) to oris subobjects. These subobjs have a property list on them which the property :scaffold=t")


(defmethod add-to-scaffold ((ori dna-origami) (scaff-obj dna))
  "Returns VALUES ori (scaffold ori) after connecting last DNA CHEM-OBJ in scaffold to scaff-obj and appends scaff-obj to (scaffold ori)"
  (with-accessors ((scaffold scaffold)) ori
    (if (null scaffold)
	(setf scaffold (list scaff-obj))
	(progn
	  (connect (car (last scaffold)) scaff-obj)
	  (setf scaffold (append scaffold(list scaff-obj)))))))


	
