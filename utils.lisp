(in-package #:small)

;; (let* ((t1  (make-instance 'dna-triangle))
;;        (af (alexandria::flatten (all-to-write t1))))
;;   ;(all-to-write t1)
;;   (overlapping-ends? (cddr (all-to-write t1))))



(defun overlapping-ends? (all)
  (let* ((1st (first all))
	 (rest (cdr all))
	 (overlaps (mapcar #'(lambda (x)
			       (overlaps? 1st x))			   
			   rest)))
    (if (cdr rest)
	(append overlaps (overlapping-ends? (cdr rest)))
	overlaps)))
	


    

(defun overlaps? (dna1 dna2)
  (cond ((eq (partner (5nt dna1)) (partner (5nt dna2)))
	 (list (partner (5nt dna1))
	       (5nt dna1)
	       (5nt dna2)
	       dna1
	       dna2
	       "DNA1: 5nt DNA2: 5nt"))
	((eq (partner (5nt dna1)) (partner (3nt dna2)))
	 (list (partner (5nt dna1))
	       (5nt dna1)
	       (3nt dna2)
	       dna1
	       dna2
	       "DNA1: 5nt DNA2: 3nt"))
	((eq (partner (3nt dna1)) (partner (3nt dna2)))
	 (list (partner (3nt dna1))
	       (3nt dna1)
	       (3nt dna2)
	       dna1
	       dna2
	       "DNA1: 3nt DNA2: 3nt"))
	((eq (partner (3nt dna1)) (partner (5nt dna2)))
	 (list (partner (3nt dna1))
	       (3nt dna1)
	       (5nt dna2)
	       dna1
	       dna2
	       "DNA1: 3nt DNA2: 5nt"))
	(t nil)))

;; (let* ((t1  (make-instance 'dna-triangle))
;;        (af (alexandria::flatten (all-to-write t1)))
;;        (four (first (subseq af 4 5)))
;;        (one (first (subseq af 1 2))))
;;   (overlapping-ends? one four))





(fset:set)





(defparameter *qqq* (make-instance 'dna-cube))



(defun double-partners? (&rest dna-objs)
  (let* ((dna-objs (alexandria:flatten dna-objs)) ;TODO? Need this for handling nested lists
	 (partners (fset:set))
	 (dbl-count 0)
	 (nts (alexandria:flatten
	       (mapcar  #'(lambda (dna-obj)
			    (let* ((nts (if (typep dna-obj 'DNA-NT)   
					    dna-obj
					    (connected-nts (5nt dna-obj)))))
			      nts))
			dna-objs))))
    (mapcar #'(lambda (nt)
		(if (and (partner nt)
			 (fset:contains? partners (partner nt)))			 
		    (progn
		      (incf dbl-count)
		      (break "Double Partner ~A" (partner nt)))
		    (setf partners (fset:with partners (partner nt)))))
	    nts)

  dbl-count))
		
(time (double-partners? (all-to-write *qqq*)))
(time (double-partners? (all-to-write (c1 *qqq*))))
(time (double-partners? (all-to-write (make-instance 'dna-cone))))
(time (double-partners? (all-to-write (make-instance 'dna-triangle))))

(let ((x (make-instance 'dna-triangle)))  
  (double-partners? (all-to-write x)))

(let ((x (make-instance 'dna-cone)))  
  (double-partners? (all-to-write x)))


(let ((x (make-instance 'dna-cone)))
  (double-partners? (stap-bridges x)))



(write-oxdna (make-instance 'dna-cone) :filename "coner")


(* 137 4)
(time 1)
