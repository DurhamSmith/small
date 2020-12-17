(in-package #:small)

;; (let* ((t1  (make-instance 'dna-triangle))
;;        (af (alexandria::flatten (all-to-write t1))))
;;   ;(all-to-write t1)
;;   (overlapping-ends? (cddr (all-to-write t1))))



(defun overlapping-ends? (all)
  (let* ((all (alexandria:flatten all))
	 (1st (first all))
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





;;(fset:set)





(defparameter *qqq* (make-instance 'dna-cube))
(write-oxdna *qqq* :filename "pflq")


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
		      (setf (base nt) (format nil "~A" dbl-count)))
;;		      (break "Double Partner ~A" (partner nt)))
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
  (double-partners? (list (first (stap-bridges x)) 
			  (third (stap-bridges x)))))

(let ((x (make-instance 'dna-cone)))
(wmdna "1st" (list (5nt x)
		   (first (stap-bridges x)))))

(let ((x (make-instance 'dna-cone)))
  (wmdna "3rd" (list (5nt x)
		     (third (stap-bridges x)))))




(let ((x (make-instance 'dna-cone)))
  (double-partners? (stap-bridges x)))



(write-oxdna (make-instance 'dna-cone) :filename "coner")
(write-oxdna (make-instance 'dna-triangle) :filename "tri")

(let* ((tri (make-instance 'dna-triangle))
       (ends (capping-ends tri :indices '(1 3 5 7 9 11 13 15 17 19 21))))
  (wmdna "tri" (all-to-write tri) ends))

(multiple-value-bind (hel nts)
 (helix-strand (v3 0 0 0)
	      (v3 1 0 0)
	      (v3 0 0 1)
	      16)
  (mapcar #'(lambda (nt b)
	      (setf (base nt) "G")
	      )
	  nts
	  '("A" "T" "C" "G" "G" "C" "T" "A" "A" "T" "C" "G" "G" "C" "T" "A"))
  (wmdna "dbl" hel 
	 (make-partner hel)
	 ))


(multiple-value-bind (hel nts)
 (helix-strand (v3 0 0 0)
	      (v3 0 0 1)
	      (v3 1 0 0)
	      16)
  (mapcar #'(lambda (nt b)
	      (setf (base nt) "G")
	      )
	  nts
	  '("A" "T" "C" "G" "G" "C" "T" "A" "A" "T" "C" "G" "G" "C" "T" "A"))
  (wmdna "dbl2" hel 
	 (make-partner hel)
	 ))

	      

(* 137 4)
(time 1)


(* 0.34 15)

(* 16 34)
(let ((h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 -1 0 0)
			16))
      (h2 (helix-strand (v3 3 5.1 0)
			(v3 0 -1 0)
			(v3 -1 0 0)
			16)))
  (connect h1 h2)
  
  (wmdna "test1" h1 (make-partner h1)
	 (make-partner h2)))


(let* ((h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 0 0 1)
			32))
       (others (loop for z from 0 to 3 collect
				       (loop for x from 0 to 3 collect
							       (duplicate-strand h1
										 :x x
										 :z z 
										 :par (evenp (+ x z))))))
       (fo (alexandria:flatten others)))
  (mapcar #'(lambda (x y)
	      (connect x y))
	  fo (cdr fo))
  (wmdna "test2" (car fo)))

(let* ((l 32)
       (sl 8)
       (trans (* (- l 1) 0.34))
       (h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 -1 0 0)
			l))

       (h2 (helix-strand (v3 3 trans 0)
			(v3 0 -1 0)
			(v3 1 0 0)
		       l))
       (stap (create-staple `((:obj ,h1 :start 0 :end ,sl :from-3end nil)
			      (:obj ,h2 :start 0 :end ,sl :from-3end t))))
       (nts1 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h1)) sl))))
       (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
       (n1 (first nts1))
       (n2 (first nts2)))
  (connect h1 h2)
  (wmdna "test3"  h1 n1 n2 stap))


(let* ((l 48)
       (sl 8)
       (trans (* (- l 1) 0.34))
       (h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 -1 0 0)
			l))

       (h2 (helix-strand (v3 3 trans 0)
			(v3 0 -1 0)
			(v3 -1 0 0)
		       l))
       (stap (create-staple `((:obj ,h1 :start 0 :end ,l :from-3end nil)
			      (:obj ,h2 :start 0 :end ,l :from-3end t))))
       (nts1 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h1)) sl))))
       (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
       (n1 (first nts1))
       (n2 (first nts2)))
  (connect h1 h2)
  (wmdna "test3"  h1 stap))






(let* ((l 32)
       (sl 8)
       (trans (* (- l 1) 0.34))
       (h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 -1 0 0)
			l))

       (h2 (helix-strand (v3 3 trans 0)
			(v3 0 -1 0)
			(v3 1 0 0)
		       l))
       (stap (create-staple `((:obj ,h1 :start 0 :end ,sl :from-3end nil)
			      (:obj ,h2 :start 0 :end ,sl :from-3end t))))
       (nts1 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h1)) 0))))
       (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
       (n1 (first nts1))
       (n2 (first nts2)))

  (wmdna "test3"  h1 n1)
  (mapcar #'(lambda (x y)
	      (list (vn x) (vn y)))
	  (connected-nts (5nt h1))
	  (reverse nts1)))








;;;; Test 4

(let* ((l 48)
       (sl 8)
       (trans (* (- l 1) 0.34))
       (h1 (helix-strand (v3 0 0 0)
			(v3 0 1 0)
			(v3 -1 0 0)
			l))
       (nts1 (connect-nts (mapcar #'make-partner (connected-nts (5nt h1)))))
       (n1 (first nts1)))
  (wmdna "input"  h1 n1))
