(in-package :small)
;(describe 'chem-obj)

(defclass/std chem-obj ()
  ((chem-objs :doc "Contains a hash-table of other CHEM-OBJs. Use  #'add-chem-obj to add to them")
   (parents :std (make-hash-table))
   (children :std (make-hash-table))
   (props :std (make-hash-table) :doc "A hashtable for abitrary properties one would like to store")
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

(defgeneric add-parent (child parent &key ckey pkey)
  (:method ((child chem-obj) (parent chem-obj)  &key pkey ckey)
    (+ht (parents child) parent :key pkey)
    (+ht (children parent) child :key ckey)))

(defgeneric add-child (parent child &key pkey ckey)
  (:method ((parent chem-obj) (child chem-obj)  &key pkey ckey)
    (+ht (parents child) parent :key pkey)
    (+ht (children parent) child :key ckey)))


(defgeneric add-prop (obj key val)
  (:method ((obj chem-obj) key val)
    (+ht (props obj) val :key key)))



(defmethod add-transformation ((obj chem-obj) tfm)
  "Prepends tfm  tfms of obj
Returns VALUES obj & list of transforms on obj"
  ;; TODO: Typechecks
   (values obj (push tfm (tfms obj))))

(defun valid-transformation? (trans)
  (if (find (car trans) '("translate" "rotate") :test #'string-equal)
      t
      (error "Not a valid transformation")))

(defun apply-transformation (tfm vec)
  "Applies a transform tfm to vector v"
  ;(format t "~& ~A ~%" tfm)
  (let ((tfm-type (car tfm))
	(tfm-val (cdr tfm)))
    (cond ((string=  "translate" tfm-type) (MAGICL::.+ tfm-val vec))
	  ((string=  "rotate" tfm-type) (MAGICL::@ tfm-val vec))
	  (t (error "Not a valid transform")))))

(defmethod apply-transformations ((obj chem-obj) v)
  "Does all the transformations that have been applied to the object in the order they were applied"
  (let* ((tfms (tfms obj))
	 (rl (reverse tfms))
	 (res (reduce #'apply-transformation tfms :initial-value  v :from-end t)))
    res))

(defmethod translate-obj ((obj chem-obj)  v)
  "prepends translation to tfms obj
Returns VALUES obj & list of transforms on obj"
  (add-transformation obj (cons "translate" v)))

(defmethod rotate-obj ((obj chem-obj) rot-mat)
    "prepends translation to tfms obj
Returns VALUES obj & list of transforms on obj"
  (add-transformation obj (cons "rotate" rot-mat)))
  
