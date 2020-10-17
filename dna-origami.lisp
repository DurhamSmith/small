(in-package :small)



(defclass/std dna-staple-strand (dna-strand)
 ())



(defmethod staple-partner ((scaff-obj dna)  &key start end from-3end)
  (make-partner scaff-obj :start start :end end :from-3end from-3end))

(defun staple-from-objs (&rest stap-objs)
  "Creates a DNA-STAPLE-STRAND which contians stap-objs in the same order"

  (setf stap-objs (alexandria:flatten stap-objs))
  (make-instance 'dna-staple-strand
		 :5nt (5nt (first stap-objs))
		 :3nt (3nt (car (last stap-objs)))))
		 

(defun create-staple (scaff-spec)
  "Creates a partner for each scaff-obj"
  (let* ((staps
	   (mapcar #'(lambda (obj-spec)
		       (staple-partner
			(getf obj-spec :obj)
			:start (getf obj-spec :start)
			:end (getf obj-spec :end)
			:from-3end (getf obj-spec :from-3end)
			))
		   scaff-spec)))
    (connect-staples staps scaff-spec)
    (values staps (connected-nts (5nt (first staps))))))


(defun connect-staples (staps scaff-spec)
;  (break "1 ~A" staps)
  (connect (first staps) (second staps))
 ; (break "2  ~A" staps)
  staps)
  

;; (defun connect-staples (staps scaff-spec)
;;   (break)
;;   (mapcar #'(lambda (h1 h2 spec1 spec2)
;; 	      (cond ((and (getf spec1 :from-3end)
;; 			  (not (getf spec2 :from-3end)))
;; ;		     (connect (3nt h1) (5nt h2))
;; 		     (progn
;; 		       ;;(break "b4 ~A ~A"  h1 h2);(strand-nts h1)  (strand-nts h2))
;; 		       (connect h1  h2)
;; 		       ;;(break "aft ~A ~A" (strand-nts  h1) (strand-nts h2))
		       
;; 		     ))

;; 		    ((and (not (getf spec1 :from-3end))
;; 			  (getf spec2 :from-3end))
;; 		     (connect  h1  h2))		  
;; 		    (t (error "Not supported"))))	  
;; 	  staps (cdr staps) scaff-spec (cdr scaff-spec)))
  

(defclass/std dna-origami (dna)
  ((scaffold :doc "The sub chem-objs defining the DNA origamis scaffold strand")
   (edge-staples :doc "The sub chem-objs defining the DNA origamis edge-staples")))
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
	  (setf scaffold (append scaffold (list scaff-obj)))))))

(defmethod add-to-edge-staples ((ori dna-origami) (obj dna))
  "Returns VALUES ori (scaffold ori) after connecting last DNA CHEM-OBJ in scaffold to obj and appends obj to (scaffold ori)"
  (with-accessors ((edge-staples edge-staples)) ori
    (if (null edge-staples)
	(setf edge-staples (list obj))
	(setf edge-staples (append edge-staples (list obj))))))


	
