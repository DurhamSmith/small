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
	 :std "?")
   (partner :doc "A DNA-NT object that forms a Watson-Crick base pair"))
  (:documentation "A class for a DNA nucleotide CHEM-OBJ. NTs are defined similary to that of oxdna using a center of mass, a vector from base to backbone and a vector normal to the face of the base. Our vn and vbb are defined OPPOSITE to that of oxdna"))

;;;; Generic Functions specific to DNA CHEM-OBJs (only exist when specilized on DNA CHEM-OBJs

(defmethod vbb ((nt dna-nt))  
  (apply-rotations nt (slot-value nt 'vbb )))

(defmethod vn ((nt dna-nt))  
  (apply-rotations nt (slot-value nt 'vn )))


(defmethod cm ((nt dna-nt))  
  (apply-transformations nt (slot-value nt 'cm )))

;;;; Generic Functions specialized on DNA CHEM-OBJs




;;;; Creation Functions
(defun make-dna-nt (&key cm vbb vn (base "?") tfms )
  ; TODO Make the arg list better. Right now things will be overwritten
  "Returns a DNA-NT CHEM-OBJ with the correctly initialized slots"
  (make-instance 'dna-nt :cm cm :vbb vbb :vn vn :base base :tfms tfms))


(defgeneric oxdna-topology (obj &key all start prev next strand inc-headers) ;todo check param list (maybe use &rest &allow-other-keys)
  (:documentation "returns the oxdna topolog of the object as a list of strings. dna/rna nucleotides will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config")
  (:method ((obj dna-nt) &key (all nil) (start 0) (prev -1) (next -1) (strand 1) (inc-headers t))
    (let* ((bases (if all
		      (reduce #'(lambda (nts nt)				  
				  (concatenate 'string nts (base nt)))
			      (connected-nts obj)
			      :initial-value "")
		      (base obj))))
      (oxdna-topology-from-seq bases :strand-num strand 
				     :start start
				     :prev prev
				     :next next
				     :inc-headers inc-headers))))


(defgeneric oxdna-config (obj &key all inc-headers)
  (:documentation "returns the oxdna configuration of the object as a (todo datatype). dna/rna nucleotides will evaluate to themselves, other structures search through (chem-obj obj) to create a nested, order list of lists of strings containing oxdna-config

if inc-headers=t returns a list
if inc-headers=nil retuns a list")
  (:method ((obj dna-nt) &key (all nil) (inc-headers t))
    (let* ((conf (if all
		     (mapcar #'(lambda (x)
				(oxdna-config-string x))
			     (connected-nts obj))
		     (oxdna-config-string obj)))
	   (header (oxdna-config-header obj :all all))
	   (conf (if (typep conf 'string) ;make list if only 1 elem, since downstream fn need lists
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
				 (u 0d0)
				 (k 0d0)
				 (box-padding (v3 1 1 1)))
  "returns a list containing the 3 oxdna header strings.
nt: dna-nt
all: bool
time: the timestep t at which the configuration has been printed
b: the length of the box sides in magicl:vector (lx ly lz) tip: (small:v3 x y z) creates these
u: potential energy
k: kinetic energy
box-padding: if b is not supplied the differecnec between the max and min x, y and z coord are added to box-padding and are used in place of b
"
  ;;todo allow to take both v3 and strings
  (let* ((tline (format nil "t = ~f" time))  ;todo check if this should be an int or real
	 (box (if all
		  (bounds (mapcar #'cm (connected-nts nt))) ; bounds returns vec of vmaxs - vmins
		  (v3 1 1 1))) ; default box size for a single nucleotide (1nm^3) todo choose better and defparameter
	 (box (if box-padding
		  (.+ box box-padding)
		  box))
	 (bline (if b
		    (format nil "b = ~f ~f ~f" (x b) (y b) (z b))
		    (format nil "b = ~f ~f ~f" (x box) (y box) (z box))))
	 (eline (format nil "E = ~f ~f ~f" (+ u k) u k)))
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
  "returns a string with the oxdna config for nt.
nt:  dna-nt
returns: string
notes: for oxdna config spec see https://dna.physics.ox.ac.uk/index.php/documentation#configuration_and_topology_files"
  (with-accessors ((cm cm) (vbb vbb) (vn vn) (v v) (l l)) nt
    (let* ((oxbb (scale vbb -1)) ;oxdna needs these vecs in the opposite direction of how we store them
	   (oxn (scale vn 1))) ;;;TODO: Look why this doesnt need to be scaled
      (concatenate 'string 
		   (print-v3 cm)
		   (print-v3 oxbb :prepend " ")
		   (print-v3 oxn :prepend " ")
		   (print-v3 v :prepend " ")
		   (print-v3 l :prepend " ")))))


(defun oxdna->file (file conf top)
  "writes a list of strings containing conf to [file].oxdna and list of strings containing conf to file.conf"
  (let* ((conf-f (concatenate 'string file ".oxdna"))
	 (top-f (concatenate 'string file ".top"))
	 (conf (alexandria:flatten conf))  ;;flatten to handle multiple config in an list
	 (top (alexandria:flatten top)))
    (write-list conf-f conf)
    (write-list top-f top)))




(defgeneric write-oxdna  (obj &key filename all start prev next strand)
  (:documentation "writes a dna chem-obj's oxdna config and topology file to [filename].oxdna and [filename].top respectively
obj: a 'dna 'chem-obj
filename: name that should be used for the config and top file ('string)
all: if t will give the topology and config for all the dna-nts connected to obj ('boolean)
start: (for topology) the starting index to be used for the .top file's first dna-nt ('integer) see: https://dna.physics.ox.ac.uk/index.php/documentation#configuration_and_topology_files
prev: (for topology) the index to be used for dna-nt before the first dna-nt in the .top file. -1 means not connected to another nt ('integer)
next: (for topology) the index to be used for dna-nt after the last dna-nt in the .top file. -1 means not connected to another nt('integer)
strand: (for topology) the strand number to be used for the .top file ('integer)")
  (:method ((obj dna-nt) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
    (let* ((conf (oxdna-config obj :inc-headers t :all all))
	   (top (oxdna-topology obj :inc-headers t
				    :all all
				    :start start
				    :prev prev
				    :next next
				    :strand strand)))
      (oxdna->file filename conf top))))
  


(defun oxdna-topology-from-seq (seq &key (strand-num 1) (start 0) (prev -1) (next -1) (inc-headers t))
  "returns values 0: (list 'string) of topologly lines 1: topology-header 'string
if inc-headers = true the header strings are prepended to the list of topology strings"
  (when (< (length seq) 1)
    (error "oxdna-topology-from-seq seq cannot be empty. it is: ~a" seq))
  (unless (typep seq 'string)
    (error "oxdna-topology-from-seq must be of type string. it is ~a" (type-of seq)))
  (let* ((len (length seq))
	 (end (+ start (- len 1)))
	 (top-header (format nil "~a ~a" len strand-num))
	 (top-lines (loop for i from start upto end collect ;todo better use of step forms
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
							   (nt-top (format nil "~a ~a ~a ~a"
									   strand-num base pnt nnt)))
						      nt-top)))
	 (top-lines (if inc-headers
			(append (list top-header) top-lines)
			top-lines)))
    (values top-lines top-header)))






(defun connected-nts (nt)
  "returns a list of dna-nt, ordered 5'->3"
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



;;; implementation of chem-objs required methods

(defun connect-nts (&rest nts)
  "dna-nt:connects all dna-nts in nts in the order they are provided"
  ;; todo: errors: not provided dna-nts, this prob done by the fact connoct errors if no valid specilizations
  (let ((nts (alexandria:flatten nts)))
    (append (mapcar #'connect nts (cdr nts)) (last nts))))







(defmethod connect ((o1 dna-nt) (o2 dna-nt) &rest rest)
  "sets (next o1) = o2 and (prev o2) = o1"
  (dna-connect o1 o2))




(defmethod next-vbb ((obj dna-nt) &key 5end kind)
  "returns a vector that would be vbb for the next dna-nt given strand type :kind"
  (with-accessors ((5nt 5nt) (3nt 3nt)) obj
    (typecase kind
      (dna-helix (next-helix-vbb obj :end 5end))
      (dna-strand (next-strand-vbb obj :end 5end))
      (t (error "(next-nt dna-nt :kind ~a) is not of valid dna-nt kind" kind)))))
    
(defmethod next-nt ((obj dna-nt) &key 5end kind)
  "returns a dna-nt that would be the next nucleotide in the sequence for a given strand type"
  (with-accessors ((5nt 5nt) (3nt 3nt)) obj
    (case kind
      (dna-helix-strand (next-helix-nt obj :5end 5end))
      (dna-single-strand (next-single-strand-nt obj :5end 5end))
      (t (error "(next-nt dna-nt :kind ~a) is not of valid dna-nt kind" kind)))))

(defun next-n-nts (nt n &key 5end kind)
  (let* ((tmp-nt (next-nt nt :5end 5end :kind kind))
	 (nts (list tmp-nt)))
    (loop for i from 2 to n do
      (progn
	(setf tmp-nt (next-nt tmp-nt :5end 5end :kind kind))
	(push tmp-nt nts)))
    (reverse nts)))


(defun partner-coords (nt)
  (with-accessors ((cm cm) (vbb vbb) (vn vn)) nt
      (let* ((pbb (scale vbb -1d0)) ;partner nt faces the other way
	     (pcm (.+ (scale pbb (* 2 *helix-cm-offset*)) cm))
					; partner is same distance from axis (axis->cm "*helix-cm-offset*
	     (pn (scale vn -1d0)))
	(values pcm pbb pn))))


(defmethod make-partner ((obj dna-nt) &key start end from-3end)
  (multiple-value-bind (cm vbb vn)
      (partner-coords obj)
    (let ((partner (make-instance 'dna-nt
				  :cm cm
				  :vbb vbb
				  :vn vn
				  :base (base-partner (base obj))
				  :partner obj)))
      (setf (partner obj) partner)
      ;(break partner)
      partner)))

(defmethod update-base ((nt dna-nt) base)
  (with-accessors ((b base) (p partner)) nt
    (setf b base)
    (when p   
      (setf (base p) (base-partner base)))))
    
  
    

(defun base-partner (base)
   (cond ((consp base) (base-partner (car base)))
	((string-equal "A" base) "T")
	((string-equal "T" base) "A")
	((string-equal "G" base) "C")
	((string-equal "C" base) "G")
	(t "!")))



(describe 'dna-nt)


(defmethod backbone ((obj dna-nt))
  (cm->bb (cm obj) (vbb obj)))

(defmethod axis ((obj dna-nt))
  (cm->axis (cm obj) (vbb obj)))


(defun nt1->nt2 (nt1 nt2)
  (.- (axis nt2)
      (axis nt1)))

(defun midpoint (nt1 nt2)
  (.+ (axis nt1)
      (scale (.- (axis nt2)
		 (axis nt1))
	     0.5)))

