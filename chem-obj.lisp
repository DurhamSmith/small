(in-package :small)


;;TODO:  Can we remove the chem-obj slot
(defclass/std chem-obj ()
  ((chem-objs :doc "Contains a hash-table of other CHEM-OBJs. Use  #'add-chem-obj to add to them")
   ;;(parents :std (make-hash-table))
   (parent :doc "A (single) parent for the chem-obj")
   (children :doc "A list of children of the chem-obj")
   ;; (children :std (make-hash-table))
   (tfms :doc "A list of transformations (translations and rotations) The order is the first applied opperation is first in the list")
   (props :std (make-hash-table) :doc "A hashtable for abitrary properties one would like to store"))
  (:documentation "The base class used for containing chemical objects and the rule for maniipulating them. They can be used to create atomic (in the lisp sense of evaluating to themselves) level detail chemical objects such as atoms or course grain nucleotide models, which have well defined coordinate descriptions. Or chem-obj children can define higher level structures composed of atomic chem-obj or other higher level structures themselves, for example small molecules composed from atoms, DNA helical strands from nucleotides and Double helices from DNA helical strands."))


;;======================================== START UNCAT FUNCTIONS ================================

(defun filter (fn 1st)
  (let ((ace nil))
    (dolist (x 1st)
      (let ((val (funcall fn x)))
	(if val (push val ace))))
    (nreverse ace)))


;TODO: Are Is add-chem-obj needed? 
(defgeneric add-chem-obj (obj val &opt key)
  (:documentation "Adds val to (chem-objs obj) under key or default key = (concatenate (type-of obj) (occurances of objs type in (chem-objs obj)))"))

;TODO: Should connect be here 
(defgeneric connect (o1 o2 &rest rest)
  (:documentation "Connects two chem-objs. Rules for these should be written for each type of connections that should be written as generic functions that specialize on them. Examples of connects are joining of nucleotides to form DNA strands or connecting nanoparticles to DNA strands")
  (:method ((o1 chem-obj) (o2 chem-obj) &rest rest)
    (error "There is no valid connection type between ~A and ~A" (class-of o1) (class-of o2))
    ))



;;======================================== END UNCAT FUNCTIONS ================================




;;======================================== START PARENT-CHILD FUNCTIONS ================================

(defgeneric add-parent (child parent)
  (:method ((child chem-obj) (parent chem-obj))
    (setf (parent child) parent)
    (push child (children parent))))

(defgeneric add-child (parent child)
  (:method ((parent chem-obj) (child chem-obj))
    (setf (parent child) parent)
    (push child (children parent))))
    


;;======================================== END PARENT-CHILD FUNCTIONS ================================


;;======================================== START PROP FUNCTIONS ================================
(defgeneric add-prop (obj key val)
  (:method ((obj chem-obj) key val)
    (+ht (props obj) val :key key)))

;TODO: Check the behaviour of this for passing (key . vals) and just keys
(defun has-props (chem-obj needed-props)
  "Returns t if chemobj contains keys and vals from props in its prop field
e.g props is an alist "
  (with-accessors ((props props)) chem-obj
    (every #'identity
	    (mapcar #'(lambda (obj-prop needed-prop)
			(equal (gethash (car needed-prop) props)
			       (cdr needed-prop))) ;TODO: Change to take a test fn in
		    (alexandria::hash-table-alist props)
		    needed-props))))

(defun find-obj-with-props (objs-list props)
  (if (listp objs-list)
      (if (has-props (car objs-list) props)
	  (car objs-list)
	  (when (cdr objs-list)
	    (find-obj-with-props (cdr objs-list) props)))
      nil))
        

;;======================================== END PROP FUNCTIONS ================================



;;======================================== START TFMS FUNCTIONS ================================


;TODO: Check that these are enough and are logically consistent and that they need to be generic functions
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


(defmethod all-tfms ((obj chem-obj))
  "Returns all transformations that should be applied to the obj
Parents transformations are applied AFTER child ones
;TODO: Add return as VALUES tfms list of parents & their tfms "
  ;(break obj)
  (let ((all-tfms (tfms obj)))
    (do ((parent (parent obj) (parent parent)))
	((null parent) all-tfms)
      (when (tfms parent)
	(setf all-tfms (append (tfms parent) all-tfms))
	))))
	   

    
(defmethod apply-transformations ((obj chem-obj) v)
  "Does all the transformations that have been applied to the object in the order they were applied"
  (let* ((tfms (all-tfms obj))
	 (rl (reverse tfms))
	 (res (reduce #'apply-transformation tfms :initial-value  v :from-end t)))
    res))

(defmethod apply-rotations ((obj chem-obj) v)
  "Does all the transformations that have been applied to the object in the order they were applied"
  (let* ((tfms (all-tfms obj))
	 (tfms (remove-if #'(lambda (x)
			     (string= "translate"
				      (car x)))
			 tfms))
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
  


;;======================================== END TFMS FUNCTIONS ================================




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Old impl. Moving to single parent, children as list not ht ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defgeneric add-parent (child parent &key ckey pkey)
;;   (:method ((child chem-obj) (parent chem-obj)  &key pkey ckey)
;;     (+ht (parents child) parent :key pkey)
;;     (+ht (children parent) child :key ckey)))

;; (defgeneric add-child (parent child &key pkey ckey)
;;   (:method ((parent chem-obj) (child chem-obj)  &key pkey ckey)
;;     (+ht (parents child) parent :key pkey)
;;     (+ht (children parent) child :key ckey)))

