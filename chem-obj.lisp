(in-package :small)

(defclass chem-obj ()
  ((subobjs :initarg :subobjs
	    :initform nil
	    :accessor subobjs
	    :allocation :instance
	    :documentation "Contains a (ordered?) list of chem-objs that make up this chem-obj (i.e. it is a higher level structure)"))
  (:documentation "The base class used for containing chemical objects and the rule for maniipulating them. They can be used to create atomic (in the lisp sense of evaluating to themselves) level detail chemical objects such as atoms or course grain nucleotide models, which have well defined coordinate descriptions. Or chem-obj children can define higher level structures composed of atomic chem-obj or other higher level structures themselves, for example small molecules composed from atoms, DNA helical strands from nucleotides and Double helices from DNA helical strands."))



