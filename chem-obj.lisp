(in-package :small)

(defclass chem-obj ()
  ((subobjs :initarg :subobjs
	    :initform nil
	    :accessor subobjs
	    :allocation :instance
	    :documentation "Contains a (ordered?) list of chem-objs that make up this chem-obj (i.e. it is a higher level structure)"))
  (:documentation "The base class used for containing chemical objects and the rule for maniipulating them. They can be used to create atomic (in the lisp sense of evaluating to themselves) level detail chemical objects such as atoms or course grain nucleotide models, which have well defined coordinate descriptions. Or chem-obj children can define higher level structures composed of atomic chem-obj or other higher level structures themselves, for example small molecules composed from atoms, DNA helical strands from nucleotides and Double helices from DNA helical strands."))


(defgeneric make-chem-obj 


(defgeneric connect (o1 o2 &key &allow-other-keys)
  (:documentation "Connects two chem-objs. Rules for these should be written for each type of connections that should be written as generic functions that specialize on them. Examples of connects are joining of nucleotides to form DNA strands or connecting nanoparticles to DNA strands")
  (:method ((o1 chem-obj) (o2 chem-obj) &key &allow-other-keys)
    (error "There is no valid connection type between ~A and ~A" (class-of o1) (class-of o2))
    ))

    
(find-class 'chem-obj)
;;(describe 'chem-obj)


;; (type-of (make-instance 'chem-obj))
;; (class-of (make-instance 'chem-obj))
