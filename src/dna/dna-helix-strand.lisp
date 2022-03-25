(in-package :small)

(defclass/std dna-helix-strand (dna-strand)
  ()
  (:documentation "A CHEM-OBJ representing a DNA double helix strand"))

(defmethod initialize-instance :after ((strand dna-helix-strand) &key)
  "Creates all DNA-NT CHEM-OBJS for the strand"
  ;; Loop for length and
  ;;; Rotate vbb and scale vaxis to get nts vbb and cm. use helixs vn and create nts with them (setting prev and next appropriately)
  )


(defun helix-axis (cm vbb)
  (.- cm (scale vbb *helix-cm-offset*)))
;(helix-axis (v3 1.6 1 0) (v3 1 0 0))


(defun next-helix-vbb (vbb vn &key 5end)
    (if 5end
	(as-unit-vec (rotate-vec vbb vn (- *rad/bp*)))
	(as-unit-vec (rotate-vec vbb vn *rad/bp*))))

(defun next-helix-cm (cm vbb vn &key 5end)
  (let* ((ax (helix-axis cm vbb))
	 (ax+1 (.+ ax (scale vn
			     (if 5end ; Choosing the direction we grow the axis
				 (- *helix-nt-spacing*)
				 *helix-nt-spacing*))))
	 (vbb+1 (next-helix-vbb vbb vn :5end 5end))
	 (cm+1 (.+ ax+1 (scale vbb+1 *helix-cm-offset*))))
    cm+1))
	
;    (let* ((axis-coords (.+ cm (scale vbb -0.6))) ;vbb points from axis->backbone and vbb
(defun next-helix-nt-coords (cm vbb vn &key 5end)
  "Returns coordinates for the next DNA-NT extending in the 3' direction (or 5' if 5end = t) as
VALUES cm vn vbb"
  (let* ((vbb+1 (next-helix-vbb vbb vn :5end 5end))
	 (cm+1 (next-helix-cm cm vbb vn :5end 5end))
	 (vn+1 (as-unit-vec vn)))
    (values cm+1 vbb+1 vn+1)))


(defun next-n-helix-vbb (vbb vn &key 5end (n 1))
    (if 5end
	(as-unit-vec (rotate-vec vbb vn (- (* n *rad/bp*))))
	(as-unit-vec (rotate-vec vbb vn (* n *rad/bp*)))))

(defun next-n-helix-cm (cm vbb vn &key 5end (n 1))
  (let* ((ax (helix-axis cm vbb))
	 (ax+n (.+ ax (scale vn
			     (if 5end ; Choosing the direction we grow the axis
				 (- (* n *helix-nt-spacing*))
				 (* n *helix-nt-spacing*)))))
	 (vbb+n (next-n-helix-vbb vbb vn :5end 5end :n n))
	 (cm+n (.+ ax+n (scale vbb+n *helix-cm-offset*))))
    cm+n))


(defun next-n-helix-nt-coords (cm vbb vn &key 5end (n 1))
  "Returns coordinates for the next DNA-NT extending in the 3' direction (or 5' if 5end = t) as
VALUES cm vn vbb"
  (let* ((vbb+n (next-n-helix-vbb vbb vn :5end 5end :n n))
	 (cm+n (next-n-helix-cm cm vbb vn :5end 5end :n))
	 (vn+n (as-unit-vec vn)))
    (values cm+n vbb+n vn+n)))



(defun next-helix-nt (nt &key 5end)
  "Returns the next DNA-NT extending in the 3' direction (or 5' if 5end = t)"
  (with-accessors ((cm cm) (vbb vbb) (vn vn)) nt
    (unless (and cm vbb vn)
      (error "cm vbb and vn need to be set for nt"))
    (multiple-value-bind (cm+1 vbb+1 vn+1)
	(next-helix-nt-coords cm vbb vn :5end 5end)
      (make-dna-nt :cm cm+1 :vbb vbb+1 :vn vn+1))))
      

(defun axis->cm (axis vbb)
  (.+ axis (scale (as-unit-vec vbb) 0.6d0)))

(defun cm->axis (cm vbb)
  (.- cm (scale (as-unit-vec vbb) 0.6d0)))

(defun cm->bb (cm vbb)
  (.+  (cm->axis cm vbb)
       (scale (as-unit-vec vbb) 1d0)))

(defun helix-strand-from-nts (nts)
  "returns a dna-single-strand containing nts.
nts: string ordered from 5'->3'"
  ;;todo type/error checking
  (let* ((first-nt (first nts))
	 (last-nt (car (last nts)))
	 (strand (make-instance 'dna-helix-strand
				:5nt first-nt
				:3nt last-nt)))
    (connect-nts nts)
    (mapcar #'(lambda (nt)
		(add-child strand nt))
	    nts)
    strand))




(defun helix-strand (coords0 vaxis vbb0 len)
  "Returns a DNA-HELIX-STRAND with len DNA-NT children, the first of whichs coords are generated by the args"
  (let* ((cm (axis->cm coords0 vbb0))
	 (nt1 (make-instance 'dna-nt
			    :cm cm
			    :vbb (as-unit-vec vbb0)
			    :vn (as-unit-vec vaxis)))
	 (nts (next-n-nts nt1 (- len 1) :kind 'DNA-HELIX-STRAND))
	 (nts (connect-nts nt1 nts))	 
	 (strand (helix-strand-from-nts nts)))
    (values strand nts)))


	   

;(defun make-helix-strand (vax vbb len &key seq tfms)
(defun make-helix-strand (vax vbb len &opt tfms seq)
  "Returns a DNA-HELIX-STRAND CHEM-OBJ with len NUCLEOTIDEs in its chem-objs class var"
  (make-instance 'dna-helix-strand
		 :v-axis vax
		 :v-bb vbb
		 :len len
		 :tfms tfms
		 :seq seq))



  
  
;; (defun next-to (strand &key (dist 3)		     
;; 			 (start 0) len
;; 			 parallel
;; 			 v x y z)
;;   "Creates a helix strand that is dist away from strand"
;;   (
(defun duplicate-strand (strand &key (dist 3) v (x 0) (y 0) (z 0) par)
  (let* ((tfm-vec (if v
                      v
                      (v3 (* dist x) (* dist y) (* dist z))))
         (s5nt (5nt strand))
         (len (length (connected-nts s5nt)))
         (svax (vn s5nt))
         (dvbb (if par
                   (vbb s5nt)
                   ;;(scale (vbb s5nt) 1.0)))
                   (scale (vbb (3nt strand)) -1.0)))

         (dvax (if par
                   svax
                   (scale svax -1.0)))
         (dcm (.+ (cm s5nt) tfm-vec))
         (dcm (if par
                  dcm
                  (.- dcm (scale dvax (* (- len 1) *helix-nt-spacing*))))))
    (helix-strand dcm dvax dvbb len)))

