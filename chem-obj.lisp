(in-package :small)
;(describe 'chem-obj)

(defclass/std chem-obj ()
  ((chem-objs :doc "Contains a hash-table of other CHEM-OBJs. Use  #'add-chem-obj to add to them")
   (tfms :doc "A list of transformations (translations and rotations) The order is the first applied opperation is first in the list"))
  (:documentation "The base class used for containing chemical objects and the rule for maniipulating them. They can be used to create atomic (in the lisp sense of evaluating to themselves) level detail chemical objects such as atoms or course grain nucleotide models, which have well defined coordinate descriptions. Or chem-obj children can define higher level structures composed of atomic chem-obj or other higher level structures themselves, for example small molecules composed from atoms, DNA helical strands from nucleotides and Double helices from DNA helical strands."))


;(defgeneric make-chem-obj 

(defgeneric add-chem-obj (obj val &opt key)
  (:documentation "Adds val to (chem-objs obj) under key or default key = (concatenate (type-of obj) (occurances of objs type in (chem-objs obj)))"))

(defgeneric connect (o1 o2 &rest rest)
  (:documentation "Connects two chem-objs. Rules for these should be written for each type of connections that should be written as generic functions that specialize on them. Examples of connects are joining of nucleotides to form DNA strands or connecting nanoparticles to DNA strands")
  (:method ((o1 chem-obj) (o2 chem-obj) &rest rest)
    (error "There is no valid connection type between ~A and ~A" (class-of o1) (class-of o2))
    ))

    
(find-class 'chem-obj)
;;(describe 'chem-obj)


;; (type-of (make-instance 'chem-obj))
;; (class-of (make-instance 'chem-obj))

(defgeneric set-parent (child parent &key ckey pkey)
  ;; (:method ((child chem-obj) (parent chem-obj))
  ;;   (setf (
  )

(defgeneric set-child (parent child &key pkey ckey)
  ;; (:method ((child chem-obj) (parent chem-obj))
  ;;   (setf (
)

