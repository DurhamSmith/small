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
(defun make-dna-nt (&key cm vbb vn (base "?") tfms)
  ; TODO Make the arg list better. Right now things will be overwritten
  "Returns a DNA-NT CHEM-OBJ with the correctly initialized slots"
  (make-instance 'dna-nt :cm cm :vbb vbb :vn vn :base base :tfms tfms))


(defgeneric oxdna-topology (obj &key all start prev next strand inc-headers) ;TODO check param list (maybe use &rest &allow-other-keys)
  (:documentation "Returns the oxdna topolog of the object as a list of strings. DNA/RNA NUCLEOTIDEs will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config")
  (:method ((obj dna-nt) &key (all nil) (start 0) (prev -1) (next -1) (strand 1) (inc-headers t))
    (let* ((bases (if all
		      (reduce #'(lambda (nts nt)
				  (concatenate 'string nts (base nt)))
			      (connected-nts obj)
			      :initial-value "")
		      (base obj))))
      (oxdna-topology-from-seq bases :start start :prev prev :inc-headers inc-headers))))


(defgeneric oxdna-config (obj &key all inc-headers)
  (:documentation "Returns the oxdna configuration of the object as a (TODO datatype). DNA/RNA NUCLEOTIDEs will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config

If inc-headers=t returns a LIST
If inc-headers=nil retuns a LIST")
  (:method ((obj dna-nt) &key (all nil) (inc-headers t))
    (let* ((conf (if all
		     (mapcar #'(lambda (x)
				(oxdna-config-string x))
			     (connected-nts obj))
		     (oxdna-config-string obj)))
	   (header (oxdna-config-header obj :all all))
	   (conf (if (typep conf 'STRING) ;make list so downstream fns can process
		     (list conf)
		     conf))
	   (conf (if inc-headers
		     (append header conf)
		     conf)))
      (values conf header))))
		    
		     
(defun oxdna-config-header (nt &key
			  (all t)
			  (time 0)
			  b
			  (U 0d0)
			  (K 0d0)
			  box-padding)
  "Returns a LIST containing the 3 oxDNA header strings.
nt: DNA-NT
all: Bool
time: the timestep T at which the configuration has been printed
b: the length of the box sides in MAGICL:VECTOR (Lx Ly Lz) Tip: (small:v3 x y z) creates these
U: Potential Energy
K: Kinetic Energy
box-padding: If b is not supplied the differecnec between the max and min x, y and z coord are added to box-padding and are used in place of b
"
  ;;TODO allow to take both v3 and strings
  (let* ((tline (format nil "t = ~f" time))  ;TODO check if this should be an int or real
	 (box (if all
		  (bounds (mapcar #'cm (connected-nts nt))) ; bounds returns vec of vmaxs - vmins
		  (v3 1 1 1))) ; Default box size for a single nucleotide (1nm^3) TODO choose better and defparameter
	 (box (if box-padding
		  (.+ box box-padding)
		  box))
	 (bline (if b
	       (format nil "b = ~f ~f ~f" (x b) (y b) (z b))
	       (format nil "b = ~f ~f ~f" (x box) (y box) (z box))))
	(eline (format nil "E = ~f ~f ~f" (+ U K) U K)))
    (list tline bline eline)))

;; (defun oxdna-box-size (nt &key
;; 			    (all t)
;; 			    (box-padding (v3 0d0 0d0 0d0) box-padding-supplied-p))
;;   (let* ((nts (if all
;; 		  (connected-nts t)
;; 		  (list nt)))
;; 	 (mins (mapcar #'lambda (x)
;; 		       ((let* ((var val))
;; 			  )

;; 		       nts)
	      
;;        )))))


(defun oxdna-config-string (nt)
  "Returns a STRING with the oxdna config for nt.
nt:  DNA-NT
Returns: String
Notes: For oxdna config spec see https://dna.physics.ox.ac.uk/index.php/Documentation#Configuration_and_topology_files"
  (with-accessors ((cm cm) (vbb vbb) (vn vn) (v v) (L L)) nt
    (let* ((oxbb (scale vbb -1)) ;oxDNA needs these vecs in the opposite direction of how we store them
	   (oxn (scale vn -1)))
      (concatenate 'string 
		   (print-v3 cm)
		   (print-v3 oxbb :prepend " ")
		   (print-v3 oxn :prepend " ")
		   (print-v3 v :prepend " ")
		   (print-v3 L :prepend " ")))))


(defun oxdna->file (file conf top)
  "Writes a LIST of STRINGS containing conf to [file].oxdna and LIST of STRINGS containing conf to file.conf"
  (let* ((conf-f (concatenate 'string file ".oxdna"))
	 (top-f (concatenate 'string file ".top")))
    (write-list conf-f conf)
    (write-list top-f top)))


(defgeneric write-oxdna  (obj &key filename all start prev next strand)
  (:documentation "Writes a DNA CHEM-OBJ's oxDNA config and topology file to [filename].oxdna and [filename].top respectively
obj: A 'DNA 'CHEM-OBJ
filename: name that should be used for the config and top file ('STRING)
all: if t will give the topology and config for all the DNA-NTs connected to obj ('BOOLEAN)
start: (for topology) the starting index to be used for the .top file's first DNA-NT ('INTEGER) SEE: https://dna.physics.ox.ac.uk/index.php/Documentation#Configuration_and_topology_files
prev: (for topology) the index to be used for DNA-NT before the first DNA-NT in the .top file. -1 means not connected to another nt ('INTEGER)
next: (for topology) the index to be used for DNA-NT after the last DNA-NT in the .top file. -1 means not connected to another nt('INTEGER)
strand: (for topology) the strand number to be used for the .top file ('INTEGER)")
  (:method ((obj dna-nt) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
    (let* ((conf (oxdna-config obj :inc-headers t :all all))
	   (top (oxdna-topology obj :inc-headers t
				    :all all
				    :start start
				    :prev prev
				    :next next
				    :strand strand)))
      (break "~A" conf)
      (oxdna->file filename conf top))))
  


(defun oxdna-topology-from-seq (seq &key (strand-num 1) (start 0) (prev -1) (next -1) (inc-headers t))
  "Returns VALUES 0: (list 'string) of topologly lines 1: topology-header 'string
If inc-headers = true the header strings are prepended to the list of topology strings"
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
						      nt-top)))
	 (top-lines (if inc-headers
			(append (list top-header) top-lines)
			top-lines)))
;    (break "~A inc ~A ~A" top-lines inc-headers (null inc-headers))
    (values top-lines top-header)))






(defun connected-nts (nt)
  "Returns a LIST of DNA-NT, ordered 5'->3"
  (let* ((orig nt)
	 (prev-nts (reverse (loop while (prev nt) 
				  do (setf nt (prev nt))
				  collect nt)))
	 (nt orig)
	 (next-nts (loop while (next nt) 
			 do (setf nt (next nt))
			 collect nt))
	 (all-nts (append prev-nts
			  (list orig)
			  next-nts)))
    all-nts))



;;; Implementation of CHEM-OBJs required methods

(defun connect-nts (&rest nts)
  "DNA-NT:CONNECTs all DNA-NTs in nts in the order they are provided"
  ;; TODO: Errors: not provided DNA-NTs, this prob done by the fact connoct errors if no valid specilizations
  (append (mapcar #'connect nts (cdr nts)) (last nts)))







(defmethod connect ((o1 dna-nt) (o2 dna-nt) &key &allow-other-keys)
  "Sets (next o1) = o2 and (prev o2) = o1"
  (setf (next o1) o2)
  (setf (prev o2) o1)
  o1)



(defun containing-strand (nt)
  "Returns an ordered list of all the DNA-NT connected to nt. The car of the list is the most (prev nt) of all DNA-NTs connected to nt"
  nt
  )
