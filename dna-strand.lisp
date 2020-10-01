(in-package :small)
;;;; NOTE: strand nt subobjs should can contain ids nt number in that strands sequence, them the strand can be gotten (traverse length) or the strand and its partner can be gotten (traverse prev next) or check if a strand is a solo strand or part of a  composite one (len = prev-next traversal len

(defclass/std dna-strand (dna)
  ((vaxis :doc "A vector pointing in the 5'->3' direction")
   (vbb :doc "A vector pointing in from the base towards the backbone")
   (vstart :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (vend :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand")
   (seq :doc "The sequence of the strand (length must be equal to len)")
   (5nt :doc "The DNA-NT at the 5'end of the DNA-STRAND")
   (3nt :doc "The DNA-NT at the 3'end of the DNA-STRAND"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

(defmethod 5end ((obj dna-strand) &key all)
  (with-accessors ((5nt 5nt)) obj
    (unless 5nt
      (error "(5end dna-strand) has no 5 end DNA-NT"))
    (let* ((5nt (if all
		    (first (connected-nts 5nt))
		    5nt))
	   (coords (cm 5nt)))
      (values 5nt coords))))

(defmethod 3end ((obj dna-strand) &key all)
  (with-accessors ((3nt 3nt)) obj
    (unless 3nt
      (error "(3end dna-strand) has no 3 end DNA-NT"))
    (let* ((3nt (if all
		    (first (connected-nts 3nt))
		    3nt))
	   (coords (cm 3nt)))
      (values 3nt coords))))
  
  
(defmethod connect ((o1 dna-strand) (o2 dna-strand) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"  
  (dna-connect o1 o2)
  (connect-nts (connected-nts o1) (connected o2)))

(defun make-dna-strand (&rest rest)
  (make-instance 'dna-strand))


(defgeneric grow (strand &key nts 5end)
  (:documentation "Returns s after adding num nt to the 3' end of strand. 
If 5end=t nt is added to the 5' end.
If nt=nil the next DNA-NT is calculated via (next-nt s)")
  (:method ((strand dna-strand) &key nts 5end)
    (typecase nts
      (integer (if (= 1 nts)
		   (grow strand :5end 5end)
		   (progn
		     (grow strand :5end 5end)
		     (grow strand :nts (- nts 1) :5end 5end))))
      (null (add-nt strand :5end 5end))
      (t (error "(grow strand) does not support type: ~A" nts)))))


(defgeneric add-nt (strand &key nt 5end)
  (:documentation "Returns (values strand nt) after adding DNA-NT nt to the 3' end of strand. If 5end=t adds it to the 5' end of strand")
  (:method ((strand dna-strand &key nt 5end))
    (typecase nt
      (if 5end
	  (connect-nts nt (nts strand))
	  (connect-nts (nts strand) nt)))
    (t (error "(grow strand) not supported for type: ~A" nt))))
		     
		  

  
 
