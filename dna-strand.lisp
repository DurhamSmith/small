(in-package :small)
;;;; NOTE: strand nt subobjs should can contain ids nt number in that strands sequence, them the strand can be gotten (traverse length) or the strand and its partner can be gotten (traverse prev next) or check if a strand is a solo strand or part of a  composite one (len = prev-next traversal len

(defclass/std dna-strand (dna)
  ((vaxis :doc "A vector pointing in the 5'->3' direction")
   (vbb :doc "A vector pointing in from the base towards the backbone")
   (vstart :doc "The coordinates of the starting point of the axis (strands 5' end)")
   (vend :doc "The coordinates of the starting point of the axis (strands 3' end)")
   (len :doc "The number of nucleotides in the strand" :std 0
	)
   (seq :doc "The sequence of the strand (length must be equal to len)")
   (5nt :doc "The DNA-NT at the 5'end of the DNA-STRAND")
   (3nt :doc "The DNA-NT at the 3'end of the DNA-STRAND"))
  (:documentation "A CHEM-OBJ representing a DNA strand, mainly used as a parent for DNA-HELIX-STRAND and DNA-SINGLE-STRAND"))

(defmethod 5end ((obj dna-strand) &key all)
  (with-accessors ((5nt 5nt)) obj
    ;; (unless 5nt
    ;;   (error "(5end dna-strand) has no 5 end DNA-NT"))
    (let* ((5nt (if all
		    (first (connected-nts 5nt))
		    5nt))
	   (coords (if 5nt
		       (cm 5nt)
		       nil)))
      (values 5nt coords))))

(defmethod 3end ((obj dna-strand) &key all)
  (with-accessors ((3nt 3nt)) obj
    ;; (unless 3nt
    ;;   (error "(3end dna-strand) has no 3 end DNA-NT"))
    (let* ((3nt (if all
		    (first (connected-nts 3nt))
		    3nt))
	   (coords (if 3nt
		       (cm 3nt)
		       nil)))
      (values 3nt coords))))
  
  
(defmethod connect ((o1 dna-strand) (o2 dna-strand) &rest rest)
  
  "Sets  o2:prev = o1, o1:next = o2 and connects their DNA-NTs"

					;  (break "~A ~A" o1 o2)
;  (connect-nts (strand-nts o1) (reverse (strand-nts o2)))
  (dna-connect (3nt o1) (5nt o2))
  (dna-connect o1 o2))
;  (connect-nts (connected-nts o1) (connected-nts o2)))
;TODO implemment full logic  (connect-nts (connected-nts o1) (connected o2)))

(defun make-dna-strand (&rest rest)
  (make-instance 'dna-strand))


(defgeneric grow (strand &key nts 5end)
  (:documentation "returns s after adding num nt to the 3' end of strand. 
if 5end=t nt is added to the 5' end.
if nt=nil the next dna-nt is calculated via (next-nt s)")
  (:method ((strand dna-strand) &key nts 5end)
    (typecase nts
      (integer (if (= 1 nts)
		    (grow strand :5end 5end)
		    (progn
		      (grow strand :5end 5end)
		      (grow strand :nts (- nts 1) :5end 5end))))
      (null (add-nt strand :5end 5end))
      (t (error "(grow strand) does not support type: ~a" nts)))))


(defgeneric add-nt (obj &key nt 5end)
  (:documentation "returns (values nt strand) after adding dna-nt nt to the 3' end of strand. if 5end=t adds it to the 5' end of strand")
  (:method ((obj dna-strand) &key nt 5end)
    (with-accessors ((5nt 5nt) (3nt 3nt)) obj
      (let* ((nt (cond ((null nt) (nt->end
				   obj
				   (next-nt obj :5end 5end)
				   :5end 5end))
		       ((typep nt 'dna-nt) (nt->end obj nt :5end 5end)))
		       (t (error "(add-nt) cant add nt of type ~a" (type-of nt))))))
	(values nt obj))))

(defgeneric nt->end (obj nt &key 5end)  
  (:documentation "returns (values obj nt) after adding a dna-nt to 3end of strand")
  (:method ((obj dna-strand) (nt dna-nt)  &key 5end)
    (if 5end
	(nt->5end obj nt)
	(nt->3end obj nt)
    )))
		 

(defmethod next-nt ((obj dna-strand) &key 5end kind)
  "Returns a DNA-NT that would be the next nucleotide in the sequence for a given strand type, defined by k"
  (with-accessors ((5nt 5nt) (3nt 3nt)) obj
    (if 5end
	(next-nt 5nt :kind (type-of obj))
	(next-nt 3nt))))

  
(defgeneric nt->3end (obj nt)  
  (:documentation "Returns (VALUES obj nt) after adding a DNA-NT to 3end of strand")
  (:method ((obj dna-strand) (nt dna-nt))
    (with-accessors ((5nt 5nt) (3nt 3nt) (len len)) obj
      (when 5nt
	(unless 3nt
	  (error "Either both or neither of 5nt & 3nt of strand should be set")))
      (when 3nt
	(unless 5nt
	  (error "Either both or neither of 5nt & 3nt of strand should be set")))
      (if 3nt
	  (progn 
	    (connect-nts 3nt nt)
	    (setf 3nt nt)
	    (incf len))
	  (progn 
	    (setf 3nt nt)
	    (setf 5nt nt)
	    (incf len)))
      (values obj nt))))

(defgeneric nt->5end (obj nt)  		;
  (:documentation "Returns (VALUES obj nt) after adding a DNA-NT to 5end of strand")
  (:method ((obj dna-strand) (nt dna-nt))
    (with-accessors ((5nt 5nt) (3nt 3nt) (len len)) obj
      (when 5nt
	(unless 3nt
	  (error "Either both or neither of 5nt & 3nt of strand should be set")))
      (when 3nt
	(unless 5nt
	  (error "Either both or neither of 5nt & 3nt of strand should be set")))
      (if 5nt
	  (progn 
	    (connect-nts nt 5nt)
	    (setf 5nt nt)
	    (incf len))
	  (progn 
	    (setf 3nt nt)
	    (setf 5nt nt)
	    (incf len)))
      (values obj nt))))
      





      
      

(defmethod oxdna-config ((obj dna-strand) &key (all nil) (inc-headers t))
  (oxdna-config (5nt obj) :inc-headers inc-headers))




(defmethod strand-nts ((strand dna-strand) &key start end from-3end)
;  (break)
  (with-accessors ((5nt 5nt) (3nt 3nt)) strand
    (let* ((nts ())
	   (nts (do ((tmp-nt 5nt (next tmp-nt)))
		    ((eq tmp-nt 3nt)
		     ;(push 3nt nts)
		     (if from-3end
			 (push 3nt nts)
			 (reverse (push 3nt nts)))
		     ) 
		  (push tmp-nt nts)))
	   (nts (if start
		    (if end
			(subseq nts start end)
			(subseq nts start))
		    nts)))
      (break "strand nts ~A" (mapcar #'base nts))
;      (break "strand nts ~A ~A" (first nts)  (car (last nts)))
;     (break "strand nts ~A ~%connected ~A" nts (connected-nts 5nt))
      nts 
      )))

(defmethod make-partner ((obj dna-strand) &key start end from-3end)
  ;;  (let* ((rnts (reverse (connected-nts (5nt obj)))) ; Reverse strand so our new strand points in the 0correct direction
  (let* ((scaff-nts (strand-nts obj
			   :start start
			   :end end
			   :from-3end from-3end)) ; Reverse strand so our new strand 
	 (nts (progn
;		(break "make-partner ~A ~A" (base (first rnts)) (base (car (last rnts))))
		(mapcar #'make-partner scaff-nts)))
	 (nts (if from-3end
		  nts
		  (reverse nts))) ; Reverse since we travese 5->3 on scaff and we need our dna to be antiparallel
	 (nts (progn
		(break "partner nts ~A" (mapcar #'base nts))
		(connect-nts nts)))
       	 (ps (make-instance (class-of obj) ; Make sure the partner is of the correct strand type 
			    :5nt (first nts)
			    :3nt (car (last nts))
			    ;; :5nt (if from-3end
			    ;; 	     (car (last nts))
			    ;; 	     (first nts))
			    ;; :3nt (if from-3end
			    ;; 	     (first nts)
			    ;; 	     (car (last nts)))
			    )))
					;    (break "~A ~A" nts ps)
    ps))
