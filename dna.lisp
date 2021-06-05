(in-package :small)

(defparameter *helix-cm-offset* 0.6d0 "Distance from helix axis to NT center of mass")

(defparameter *helix-spacing* 1d0 "Spacing between parallel double helices")

(defparameter *helix-diameter* 2d0 "Diameter of DNA double helix in nm")
;TODO: Remove and update codebase
(defparameter *helix-radius* (/ *helix-diameter* 2d0) "Radius of DNA double helix in nm")
;TODO: Check where this is needed and update codebase
(defparameter *inter-helix-spacing* 1d0 "Spacing between two helices in an origmi design")

(defparameter *helix-nt-spacing* 0.34d0 "Length in nm between successive bases in a double helix")

(defparameter *single-strand-nt-spacing* 0.4d0 "Length in nm between successive bases in an unpaired DNA strand")

;TODO: Maybe provide convienice functions for setting things with a relationship between them
(defparameter *bp/turn* 10.44d0 "The number of bases per helical turn")

(defparameter *rad/bp* (/ (* pi 2) *bp/turn*) "The delta theta between backbone vectors when viewed down the axis in the 5'->3' direction (- theta-nt1 theta-nt0)")

(defclass/std dna (chem-obj)
  ((5nt :doc "The DNA-NT at the 5'end")
   (3nt :doc "The DNA-NT at the 3'end")
   (prev :doc "The previous DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included")
   (next :doc "The next DNA CHEM-OBJ in dna sequence. Together with (next dna) form a doubly linked list of DNA CHEM-OBJS used to represent a grouped sequence of these DNA CHEM-OBJs, which themselves could be at various levels of abstraction (e.g strand composed of nucleotides, or origami scaffolds composed of single and helical strands). These sequences are on the same molecule, i.e. partner strands are not included")
   (partner :doc "A DNA-NT object that forms a Watson-Crick base pair"))
  (:documentation "A class for DNA chem-objs. Defines constants and connect methods"))

;(describe 'dna)

;TODO: Should I add a destructive keyword?
(defgeneric make-partner (obj &key start end from-3end)
  (:documentation "Return a complementary DNA CHEM-OBJ for the given DNA CHEM-OBJ")
  (:method ((obj dna)  &key start end from-3end)
    (error "generic function #'make-partner not implemented for ~A" (class-of obj))))

;TODO: Make connect do tail traversal so we can connect a list of dna objects
;TODO: Check that adding the generic fallback is sensible
(defgeneric connect (dna1  dna2 &rest rest)
(:documentation "Return a complementary DNA CHEM-OBJ for the given DNA CHEM-OBJ")
  (:method ((dna1 dna) (dna2 dna)  &key start end from-3end)
    (format t "generic function #'connect relying on default implemented for DNA")
    (dna-connect dna1 dna2)))


;;TODO: Can this be a function?
(defmethod dna-connect ((o1 dna) (o2 dna))
  "Returns VAUES o1 o2 after setting o2:prev = o1, o1:next = o2"
  (setf (next o1) o2)
  (setf (prev o2) o1)
  (values o1 o2))

(defgeneric nts (obj &key all)
  (:documentation "Gets DNA-NTs from dna CHEM-OBJ. If all = t all connected nts are returned if all all = nil then only the DNA-NTs in the strand are returned"))


;TODO: Can this be moved to just a function? What are the use cases for things like calling it on a dna strand?
(defgeneric next-nt (obj &key 5end kind)
  (:documentation "Returns a DNA-NT that would be the next nucleotide in the sequence for a given strand type")
  (:method (obj &key 5end kind)
    (error "(next-nt (obj ~A ) &key 5end) has not been implemented ~A" (type-of obj) obj)))

;TODO: Are these needed?
(defgeneric 5end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))

(defgeneric 3end (obj &key all)
  (:documentation "Returns (VALUES DNA-NT vector-with-dna-nts-axis-coords) of the 5 prime end."))



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
		 (let* ((nt (if (typep dna-obj 'DNA-NT)    ;;TODO Changed this to print single nts but need to fix more
				dna-obj
				(5nt dna-obj)))
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
    "b = 1000.0000 1000.0000 1000.0000"  
    "E = 0.000000 0.000000 0.000000"))

    
    
		   
  

    
