(in-package :small)
;;;; NOTE: strand nt subobjs should can contain ids nt number in that strands sequence, them the strand can be gotten (traverse length) or the strand and its partner can be gotten (traverse prev next) or check if a strand is a solo strand or part of a  composite one (len = prev-next traversal len

(defclass/std dna-strand (dna)
  ((vaxis :doc "A vector pointing in the 5'->3' direction")
   (vbb :doc "A vector pointing in from the base towards the backbone")
   (vstart :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (vend :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand")
   (seq :doc "The sequence of the strand (length must be equal to len)"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

(defmethod connect ((o1 dna-strand) (o2 dna-strand) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"  
  (dna-connect o1 o2)
  (connect-nts (connected-nts o1) (connected o2)))

(defun make-dna-strand (&rest rest)
  (make-instance 'dna-strand))


(defgeneric grow (strand &key nts head)
  (:documentation "Returns s after adding num nt to the 3' end of strand. 
If head=t nt is added to the 5' end.
If nt=nil the next DNA-NT is calculated via (next-nt s)")
  (:method ((strand dna-strand) &key nts head)
    (typecase nts
      (integer (if (= 1 nts)
		   (grow strand :head head)
		   (progn
		     (grow strand :head head)
		     (grow strand :nts (- nts 1) :head head))))
      (null (add-nt strand :head head))
      (t (error "(grow strand) does not support type: ~A" nts)))))


(defgeneric add-nt (strand &key nt head)
  (:documentation "Returns (values strand nt) after adding DNA-NT nt to the 3' end of strand. If head=t adds it to the 5' end of strand")
  (:method ((strand dna-strand &key nt head))
    (typecase nt
      (if head
	  (connect-nts nt (nts strand))
	  (connect-nts (nts strand) nt)))
    (t (error "(grow strand) not supported for type: ~A" nt))))
		     
		  

  
 
(typep 'null nil)
