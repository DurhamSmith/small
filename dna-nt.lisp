(in-package :small)
;;;; This a base class for DNA CHEM-OBJs.
(defclass/std dna-nt (dna)
  ((cm :doc "Vector of the nucleotides center of mass coordinates")
   (vbb :doc "Unit vector pointing from base->backbone")
   (vn :doc "Unit vector normal to the face of the DNA base in the 5'->3' direction")
   (v :doc "The velocity"
      :std (v3 0 0 0))
   (L :doc "The angular velocity"
      :std (v3 0 0 0))
   (base :doc "The Watson-Crick base of the DNA-NT"
	 :std "?"))
  (:documentation "A class for a DNA nucleotide CHEM-OBJ. NTs are defined similary to that of oxdna using a center of mass, a vector from base to backbone and a vector normal to the face of the base. Our vn and vbb are defined OPPOSITE to that of oxdna"))


;;;; Generic Functions specific to DNA CHEM-OBJs (only exist when specilized on DNA CHEM-OBJs



;;;; Generic Functions specialized on DNA CHEM-OBJs


;;;; Creation Functions
(defun make-dna-nt (&key cm vbb vn base tfms)
  "Returns a DNA-NT CHEM-OBJ with the correctly initialized slots"
  (make-instance 'dna-nt :cm cm :vbb vbb :vn vn :base base :tfms tfms))

(defgeneric oxdna-config (obj &key &allow-other-keys)
  (:documentation "Returns the oxdna configuration of the object as a (TODO datatype). DNA/RNA NUCLEOTIDEs will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config")
  (:method ((obj dna-nt) &key &allow-other-keys)
    (with-accessors ((cm cm) (vbb vbb) (vn vn) (v v) (L L)) obj
      (let* ((oxbb (scale vbb -1)) ;oxDNA needs these vecs in the opposite direction of how we store them
	     (oxn (scale vn -1)))
	(concatenate 'string 
		     (print-v3 cm)
		     (print-v3 oxbb :prepend " ")
		     (print-v3 oxn :prepend " ")
		     (print-v3 v :prepend " ")
		     (print-v3 L :prepend " "))))))




(defun oxdna-topology-from-seq (seq &key (strand-num 1) (start 0) (prev -1) (next -1))
  "Returns VALUES 0: (list 'string) of topologly lines 1: topology-header 'string"
  (when (< (length seq) 1)
    (error "oxdna-topology-from-seq seq cannot be empty. It is: ~A" seq))
  (unless (typep seq 'STRING)
    (error "oxdna-topology-from-seq must be of type STRING. It is ~A" (type-of seq)))
  (let* ((len (length seq))
	 (end (+ start (- len 1)))
	 (top-header (format nil "~A ~A" len strand-num))
	 (top-lines (loop for i from start upto end collect ;TODO better use of step forms
					      (let* ((i+1 (+ i 1))
						     (i-1 (- i 1))
						     (pnt (if (= i start)
							      prev
							      i-1))
						     (nnt (if (= i end)
							      next
							      i+1))
						     (base (subseq seq
								   (- i start)
								   (- i+1 start)))
						     (nt-top (format nil "~A ~A ~A ~A"
								     strand-num base pnt nnt)))
						nt-top))))
    (values top-lines top-header)))



(fmakunbound 'oxdna-topology)
(defgeneric oxdna-topology (obj &key all start prev next strand) ;TODO check param list (maybe use &rest &allow-other-keys)
  (:documentation "Returns the oxdna topolog of the object as a list of strings. DNA/RNA NUCLEOTIDEs will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config")
  (:method ((obj dna-nt) &key (all nil) (start 0) (prev -1) (next -1) (strand 1))
    (with-accessors ((base base)) obj
      (oxdna-topology-from-seq base :start start :prev prev))))















;;; Implementation of CHEM-OBJs required method



(defun connect-nts (&rest nts)
  "DNA-NT:CONNECTs all DNA-NTs in nts in the order they are provided"
  ;; TODO: Errors: not provided DNA-NTs, this prob done by the fact connoct errors if no valid specilizations
  (append (mapcar #'connect nts (cdr nts)) (last nts)))



;;;; THIS IS MORE FOR TRAVERSAL
;; (defun connect-nts (&rest nts)
;;   "DNA-NT:CONNECTs all DNA-NTs in nts in the order they are provided" ;TODO better list traversal
;;   ;; TODO: Errors: not provided DNA-NTs, this prob done by the fact connoct errors if no valid specilizations
;;   (when (< (length nts) 2)
;;     (error "DNA-NT:CONNECT-NTS requires at least two nucleotides. Got: ~A" nts))
;;   (let* ((orig (first nts))
;; 	 (nt orig)
;; 	 (prev-nts (loop while (prev nt) 
;; 			 do (setf nt (prev nt))
;; 			 collect (nt)))
;; 	 (nt orig)
;; 	 (next-nts (loop while (next nt) 
;; 			 do (setf nt (next nt))
;; 			 collect (nt)))
;; 	 (all-nts (append prev-nts
;; 			  (list orig)
;; 			  next-nts)))
;;     all-nts))



(defmethod connect ((o1 dna-nt) (o2 dna-nt) &key &allow-other-keys)
  "Sets (next o1) = o2 and (prev o2) = o1"
  (setf (next o1) o2)
  (setf (prev o2) o1)
  o1)



(defun containing-strand (nt)
  "Returns an ordered list of all the DNA-NT connected to nt. The car of the list is the most (prev nt) of all DNA-NTs connected to nt"
  nt
  )
