(in-package :small)


;;TODO: Do we need this? or can we get away with using only helix strands? Or is there a better abstraction (maybe one that contains a list of helical strands that define the partners?
(defclass/std dna-staple-strand (dna-strand)
 ())



(defmethod staple-partner ((scaff-obj dna)  &key start end from-3end)
  (make-partner scaff-obj :start start :end end :from-3end from-3end))

(defun staple-from-objs (&rest stap-objs)
  "Creates a DNA-STAPLE-STRAND which contians stap-objs in the same order"
  ;; TODO maybe connect them after flatten?
  (let* ((stap-objs (alexandria:flatten stap-objs))
	 (stap (make-instance 'dna-staple-strand
			       :5nt (5nt (first stap-objs))
			       :3nt (3nt (car (last stap-objs))))))
    (mapcar #'(lambda (stap-obj)
		(add-parent stap-obj stap))
	    stap-objs)
    stap))




(defun staple-from-spec (spec)
  "Creates a staple strand from spec.
spec: (:obj DNA  :start INT :end INT  :from-3end BOOL) of
(:single-strand t [opt] :num-nts INT/nil)  if nil num-nts calculated automatically)")


(defun create-staple (scaff-spec)
  "Creates a partner for each scaff-obj"
  (let* ((helix-staps
	   (mapcar #'(lambda (obj-spec)
		       (when (getf obj-spec :obj)
			 (staple-partner
			  (getf obj-spec :obj)
			  :start (getf obj-spec :start)
			  :end (getf obj-spec :end)
			  :from-3end (getf obj-spec :from-3end)
			  )))
		   scaff-spec))
	 (single-strands
	   (loop
	     for i from 0 to (- (length scaff-spec) 1)
	     collect
	     (let* ((spec (nth i scaff-spec))
		    (vbb (v3 0 1 1))
		    prev-hel next-hel)
	       (when (getf spec :single-strand)
		 (let ((prev-nt (cm->bb (cm (3nt (nth (- i 1) helix-staps )))
					(vbb (3nt (nth (- i 1) helix-staps )))))
		       (next-nt (cm->bb (cm (5nt (nth (+ i 1) helix-staps)))
					(vbb (5nt (nth (+ i 1) helix-staps)))))
		       (num-nts (getf spec :num-nts)))
		   (bridging-single-strand prev-nt next-nt vbb :len num-nts))))))
	 (staps
	   ))
    (setf staps 
	  (loop
	    for i from 0 to (- (length scaff-spec) 1)
	    collect
	    (cond ((getf (nth i scaff-spec) :obj)
		   (nth i helix-staps))
		  ((getf (nth i scaff-spec) :single-strand)			  
		   (nth i single-strands))
		  (t (error "Not supported")))))
	     
    ;(break "~A ~% SS  ~A ~% ALL ~A" helix-staps single-strands staps)
    (connect-staples staps)
    (values (staple-from-objs staps) (connected-nts (5nt (first staps))))))


;; (defun create-staple (scaff-spec)
;;   "Creates a partner for each scaff-obj"
;;   (let* ((staps
;; 	   (mapcar #'(lambda (obj-spec)
;; 		       (staple-partner
;; 			(getf obj-spec :obj)
;; 			:start (getf obj-spec :start)
;; 			:end (getf obj-spec :end)
;; 			:from-3end (getf obj-spec :from-3end)
;; 			))
;; 		   scaff-spec)))
;;     (connect-staples staps)
;;     (values (staple-from-objs staps) (connected-nts (5nt (first staps))))))


(defun connect-staples (staps)
;  (break "1 ~A" staps)
  ;;(connect (first staps) (second staps))
 (mapcar #'connect staps (cdr staps))
 ; (break "2  ~A" staps)
  staps)
  
;;TODO: Recheck this and see in we need edge-staples

(defclass/std dna-origami (dna)
  ((scaffold :doc "The sub chem-objs defining the DNA origamis scaffold strand")
   (edge-staples :doc "The sub chem-objs defining the DNA origamis edge-staples")
   (5nt :doc "The DNA-NT at the 5'end of the DNA-STRAND")
   (3nt :doc "The DNA-NT at the 3'end of the DNA-STRAND")))
;; )
;;   (:documentation "This class defines a DNA origami object. Its scaffold strand is defined as a list similar to subobjs. Its subobjs contain the other dna elements such as edge strands connectorn and staple strands/briges"))


;; (defgeneric add-staples (ori staples)
;;   "Adds a list of origami staples (DNA-STRAND) to oris subobjects. These subobjs have a property list on them which the property :scaffold=t")


(defmethod add-to-scaffold ((ori dna-origami) (scaff-obj dna))
  "Returns VALUES ori (scaffold ori) after connecting last DNA CHEM-OBJ in scaffold to scaff-obj and appends scaff-obj to (scaffold ori)"
  (with-accessors ((scaffold scaffold)) ori
    (if (null scaffold)
	(setf scaffold (list scaff-obj)
	      (5nt ori) (5nt scaff-obj)) ; We set the 5nt if it is the firs obj in the scaffold)
	(progn
	  (connect (car (last scaffold)) scaff-obj)
	  (setf scaffold (append scaffold (list scaff-obj)))))
    ;;We need to set origami as the parent to the scaff-obj
    (setf (3nt ori) (3nt scaff-obj))
    (add-parent scaff-obj ori)
    ori))

(defmethod add-to-edge-staples ((ori dna-origami) (obj dna))
  "Returns VALUES ori (scaffold ori) after connecting last DNA CHEM-OBJ in scaffold to obj and appends obj to (scaffold ori)"
  (with-accessors ((edge-staples edge-staples)) ori
    (if (null edge-staples)
	(setf edge-staples (list obj))
	(setf edge-staples (append edge-staples (list obj))))))


	

;;;; Generic implementation for connect for origami
(defmethod connect ((o1 dna-origami) (o2 dna-origami) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"

					;  (break "~A ~A" o1 o2)
;  (connect-nts (strand-nts o1) (reverse (strand-nts o2)))
  (dna-connect (3nt o1) (5nt o2))
  (dna-connect o1 o2))

(defmethod connect ((o1 dna-strand) (o2 dna-origami) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"

					;  (break "~A ~A" o1 o2)
;  (connect-nts (strand-nts o1) (reverse (strand-nts o2)))
  (dna-connect (3nt o1) (5nt o2))
  (dna-connect o1 o2))

(defmethod connect ((o1 dna-origami) (o2 dna-strand) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"

					;  (break "~A ~A" o1 o2)
;  (connect-nts (strand-nts o1) (reverse (strand-nts o2)))
  (dna-connect (3nt o1) (5nt o2))
  (dna-connect o1 o2))

(defun update-scaffold-bases (ori bases)
  "Take a dna-origami object and a string containing the new bases for the scaffold and sets the scaffold bases to this string"
  (let ((nts (connected-nts (5nt ori))))
    (mapcar #'update-base nts (map 'list #'string  bases))))
