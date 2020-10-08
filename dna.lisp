(in-package :small)



(defparameter *helix-spacing* 1d0 "Spacing between parallel double helices in this design")

(defparameter *helix-diameter* 2d0 "Diameter of DNA double helix in nm")

(defparameter *inter-helix-spacing* 1d0 "Spacing between two helices in an origmi design")

(defparameter *helix-nt-spacing* 0.34d0 "Length of each base pair in a double helix in nm")

(defparameter *single-strand-nt-spacing* 0.4d0 "Length of each base pair in a single in nm")

(defparameter *bp/turn* 10.44d0 "Length of each base pair in a double helix in nm")

(defparameter *rad/bp* (/ (* pi 2) *bp/turn*) "The delta theta between backbone vectors when viewed down the axis in the 5'->3' direction (- theta-nt1 theta-nt0)")



(defclass/std dna (chem-obj)
  ((prev :doc "The previous DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included")
   (next :doc "The next DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included"))
  (:documentation "A class for DNA chem-objs. Defines constants and connect methods"))

;(describe 'dna)


(defgeneric make-partner (obj &key start end from-3end)
  (:documentation "Return a complementary DNA CHEM-OBJ for the given DNA CHEM-OBJ")
  (:method ((obj dna)  &key start end from-3end)
    (error "generic function #'make-partner not implemented for ~A" (class-of obj))))


(defmethod dna-connect ((o1 dna) (o2 dna))
  "Returns VAUES o1 o2 after setting o2:prev = o1, o1:next = o2"
  (setf (next o1) o2)
  (setf (prev o2) o1)
  (values o1 o2))

(defgeneric nts (obj &key all)
  (:documentation "Gets DNA-NTs from dna CHEM-OBJ. If all = t all connected nts are returned if all all = nil then only the DNA-NTs in the strand are returned"))



(defgeneric next-nt (obj &key 5end kind)
  (:documentation "Returns a DNA-NT that would be the next nucleotide in the sequence for a given strand type")
  (:method (obj &key 5end kind)
    (error "(next-nt (obj ~A ) &key 5end) has not been implemented ~A" (type-of obj) obj)))


(defgeneric 5end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))

(defgeneric 3end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))

(defgeneric partner (obj)
  (:documentation "Returns a partner of the DNA CHEM-OBJ"))




(defun wmdna (filename &rest dna-objs)
  "Write oxdna of multiple dna objs"
  (let* ((dna-objs (alexandria:flatten dna-objs)) ;TODO? Need this for handling nested lists
	 (nt-num 0)
	 (strand-num 1)
	 (configs '())
	 (tops '())
	 (top-header '())
	 (config-header '()))
    (mapcar  #'(lambda (dna-obj)
		 (let* ((nt (5nt dna-obj))
			(len (length (connected-nts nt))))
		   ;; (setf configs (append configs (oxdna-config nt
		   ;; 					       :inc-headers nil
		   ;; 					       :all t)))
		   (push (oxdna-config nt
				       :inc-headers nil
				       :all t)
			 configs)
		   (push (oxdna-topology nt
					 :inc-headers nil
					 :strand strand-num
					 :start nt-num
					 :all t)
			 tops)
		   ;; (setf tops (append tops (oxdna-topology nt
		   ;; 					   :strand strand-num
		   ;; 					   :start nt-num
		   ;; 					   :all t)))
		   
		   (incf nt-num len)
		   (incf strand-num)))
	     dna-objs)
    (setf configs (reverse configs))
    (setf tops (reverse tops))
    (setf tops (append (compound-topology-header tops) tops))
    (setf configs (append (compound-config-header configs) configs))
    (oxdna->file filename configs tops)
    ))

(defun compound-topology-header (tops)
  "Takes a list of oxdna topologies and returns a compound header for them" 
  (let* ((num-strands(length tops))
	 (num-nts 0))
    (mapcar #'(lambda (top)
		(incf num-nts (length top)))
	    tops)
    (list (format nil "~A ~A" num-nts num-strands))))

(defun compound-config-header (configs)
;;;TODO implement this better
  '("t = 0"
    "b = 100.0000 100.0000 100.0000"  
    "E = 0.000000 0.000000 0.000000"))

    
    
		   
  

    
