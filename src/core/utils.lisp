(in-package #:small)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             String Utils            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string->list (string)
 (loop for c across string collect  (string c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             Other Utils             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group (source n)
  (if (endp source)
      nil
      (let ((rest (nthcdr n source)))
        (cons (if (consp rest) (subseq source 0 n) source)
              (group rest n)))))

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



(defun deg->rad (deg)
  (* (/ pi 180) deg))

(defun rad->deg (rad)
  (* (/ 180 pi) rad))





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
		
;; (time (double-partners? (all-to-write *qqq*)))
;; (time (double-partners? (all-to-write (c1 *qqq*))))
;; (time (double-partners? (all-to-write (make-instance 'dna-cone))))
;; (time (double-partners? (all-to-write (make-instance 'dna-triangle))))

;; (let ((x (make-instance 'dna-triangle)))  
;;   (double-partners? (all-to-write x)))

;; (let ((x (make-instance 'dna-cone)))  
;;   (double-partners? (all-to-write x)))


;; (let ((x (make-instance 'dna-cone)))
;;   (double-partners? (list (first (stap-bridges x)) 
;; 			  (third (stap-bridges x)))))

;; (let ((x (make-instance 'dna-cone)))
;; (wmdna "1st" (list (5nt x)
;; 		   (first (stap-bridges x)))))

;; (let ((x (make-instance 'dna-cone)))
;;   (wmdna "3rd" (list (5nt x)
;; 		     (third (stap-bridges x)))))




;; (let ((x (make-instance 'dna-cone)))
;;   (double-partners? (stap-bridges x)))






;; (let* ((tri (make-instance 'dna-triangle))
;;        (ends (capping-ends tri :indices '(1 3 5 7 9 11 13 15 17 19 21))))
;;   (wmdna "tri" (all-to-write tri) ends))

;; (multiple-value-bind (hel nts)
;;  (helix-strand (v3 0 0 0)
;; 	      (v3 1 0 0)
;; 	      (v3 0 0 1)
;; 	      16)
;;   (mapcar #'(lambda (nt b)
;; 	      (setf (base nt) "G")
;; 	      )
;; 	  nts
;; 	  '("A" "T" "C" "G" "G" "C" "T" "A" "A" "T" "C" "G" "G" "C" "T" "A"))
;;   (wmdna "dbl" hel 
;; 	 (make-partner hel)
;; 	 ))


;; (multiple-value-bind (hel nts)
;;  (helix-strand (v3 0 0 0)
;; 	      (v3 0 0 1)
;; 	      (v3 1 0 0)
;; 	      16)
;;   (mapcar #'(lambda (nt b)
;; 	      (setf (base nt) "G")
;; 	      )
;; 	  nts
;; 	  '("A" "T" "C" "G" "G" "C" "T" "A" "A" "T" "C" "G" "G" "C" "T" "A"))
;;   (wmdna "dbl2" hel 
;; 	 (make-partner hel)
;; 	 ))

	      

;; (* 137 4)
;; (time 1)


;; (* 0.34 15)

;; (* 16 34)
;; (let ((h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			16))
;;       (h2 (helix-strand (v3 3 5.1 0)
;; 			(v3 0 -1 0)
;; 			(v3 -1 0 0)
;; 			16)))
;;   (connect h1 h2)
  
;;   (wmdna "test1" h1 (make-partner h1)
;; 	 (make-partner h2)))


;; (let* ((h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 0 0 1)
;; 			32))
;;        (others (loop for z from 0 to 3 collect
;; 				       (loop for x from 0 to 3 collect
;; 							       (duplicate-strand h1
;; 										 :x x
;; 										 :z z 
;; 										 :par (evenp (+ x z))))))
;;        (fo (alexandria:flatten others)))
;;   (mapcar #'(lambda (x y)
;; 	      (connect x y))
;; 	  fo (cdr fo))
;;   (wmdna "test2" (car fo)))

;; (let* ((l 32)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))

;;        (h2 (helix-strand (v3 3 trans 0)
;; 			(v3 0 -1 0)
;; 			(v3 1 0 0)
;; 		       l))
;;        (stap (create-staple `((:obj ,h1 :start 0 :end ,sl :from-3end nil)
;; 			      (:obj ,h2 :start 0 :end ,sl :from-3end t))))
;;        (nts1 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h1)) sl))))
;;        (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
;;        (n1 (first nts1))
;;        (n2 (first nts2)))
;;   (connect h1 h2)
;;   (wmdna "test3"  h1 n1 n2 stap))

;; (let* ((l 48)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))

;;        (h2 (helix-strand (v3 3 trans 0)
;; 			(v3 0 -1 0)
;; 			(v3 -1 0 0)
;; 		       l))
;;        (stap (create-staple `((:obj ,h1 :start 0 :end ,l :from-3end nil)
;; 			      (:obj ,h2 :start 0 :end ,l :from-3end t))))
;;        (nts1 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h1)) sl))))
;;        (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
;;        (n1 (first nts1))
;;        (n2 (first nts2)))
;;   (connect h1 h2)
;;   (wmdna "test3"  h1 stap))






;; (let* ((l 48)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))

;;        (h2 (helix-strand (v3 3 trans 0)
;; 			(v3 0 -1 0)
;; 			(v3 1 0 0)
;; 		       l))
;;        (stap (create-staple `((:obj ,h1 :start 0 :end ,sl :from-3end nil)
;; 			      (:obj ,h2 :start 0 :end ,sl :from-3end t))))
;;        (nts1 (connect-nts (reverse (mapcar #'make-partner (subseq (connected-nts (5nt h1)) 0)))))
;;        (nts2 (connect-nts (mapcar #'make-partner (subseq (connected-nts (5nt h2)) 0 (- l sl)))))
;;        (n1 (first nts1))
;;        (n2 (first nts2)))

;;   (wmdna "input"  h1 n1)
;;   (mapcar #'(lambda (x y)
;; 	      (list (cm x) (cm y)))
;; 	  (connected-nts (5nt h1))
;; 	   nts1))








;; ;;;; Test 4

;; (let* ((l 48)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))
;;        (nts1 (connect-nts (mapcar #'make-partner (connected-nts (5nt h1)))))
;;        (n1 (first nts1)))
;;   (wmdna "input"  h1 n1))

;; (let* ((l 48)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))
;;        (p1 (make-partner h1)))
;;   (wmdna "input"  h1 p1))



;; (let* ((l 8)
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))
;;        (nts1 (connect-nts (mapcar #'make-partner (connected-nts (5nt h1)))))
;;        (n1 (first nts1)))
;;   (wmdna "input1"  h1 n1))

;; (let* ((l 8)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			(v3 0 1 0)
;; 			(v3 -1 0 0)
;; 			l))
;;        (p1 (make-partner h1)))
;;   (wmdna "input2"  h1 p1))

;; (let* ((l 8)
;;        (sl 8)
;;        (trans (* (- l 1) 0.34))
;;        (h1 (helix-strand (v3 0 0 0)
;; 			 (v3 0 1 0)
;; 			 (v3 -1 0 0)
;; 			 l))
;;        (p1 (create-staple `((:obj ,h1  :start 0 :end 8 :from-3end t)))))
;;   (wmdna "input-3t"  h1 p1))

;; (SMALL::create-staple `((:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end nil)
;; 			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end t)
;; 				(:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end nil)))


;; (let* ((tri (make-instance 'dna-triangle))
;;        (stp (first (joining-strands tri)))
;;        (cs (first (capping-staps tri)))
;;        (s1 (first (scaffold tri)))
;;        (s2 (second (scaffold tri)))
;;        (s5 (nth 6 (scaffold tri)))
;;        (sng (alexandria:flatten (loop for i from 2 to 31 by 3 collect
;; 					  (children (nth i (scaffold tri))))))
       
;;        (all (connected-nts (5nt s1)))
;;        (x (mapc #'(lambda (x)
;; 			 (delete x all))
;; 		     sng))
;;        (more (reverse (mapcar #'(lambda (x)
;; 	      (unless (partner x) (make-partner x)))
;; 			      all)))
;;    ;; ;    )
;;        (m (mapcar #'(lambda (x y)
;; 		      (when y (connect-nts x y)))
;; 		  more (cdr more)))
;;        (more (remove nil more)))
;; ;;  more)
;; ;  (setf (next (3nt s5)) nil )
;;   ;;  (break (first (internal-staps tri))))
  
;;   (wmdna "tri" x))

;; (let* ((tri (make-instance 'dna-triangle)))
;;   (make-partner (first (scaffold tri)))
;;   ;(wmdna "tri" (mapcar #'make-partner (remove-if #'(lambda (x) (typep x 'dna-single-strand )) (scaffold tri)))))
;;   (wmdna "tri" (mapcar #'make-partner (remove-if #'(lambda (x) (typep x 'dna-single-strand )) (scaffold tri))) (5nt tri)))
       

;; (all-to-write tri) more)
;;   (wmdna "input" s1 (partner (5nt s1)) (subseq (first (internal-staps tri)) 0 2 ))

;; (break (make-instance 'dna-triangle))



;; (write-oxdna (make-instance 'dna-triangle) :filename "tri")

;; (mapcar #'(lambda (x y)
;; 	    (format t "~& ~A ~A ~%" x y))
;; 	'(1 2 3 4) '(a b c d))


;; (write-oxdna (make-instance 'dna-cone) :filename "coner")
;; (write-oxdna (5nt (make-instance 'dna-cone)) :filename "coner-scaff")
;; (write-oxdna (make-instance 'dna-triangle) :filename "tri")
;; (write-oxdna (make-instance 'dna-cube) :filename "cube")
;; (joining-strands-as-idt (make-instance 'dna-triangle) "hi")
;; (strands-as-idt "hi" (make-instance 'dna-triangle))
;; (as-idt-seq (make-instance 'dna-triangle))


;; (stap-bridges-as-idt (make-instance 'dna-triangle) "staple-bridges")
;; (joining-strands-as-idt (make-instance 'dna-cube) "joining-strands")
;; (joining-strands-as-idt (make-instance 'dna-triangle) "joining-strands")
;; (joining-strands (t1 (c1 (make-instance 'dna-cube))))
;; (make-instance 'dna-cone)

;; (as-idt (make-instance 'dna-cube))
;; (strands-as-idt "internal" (internal-staps (make-instance 'dna-triangle)))
;; (internal-staps-as-idt (make-instance 'dna-cone) "cone1")

;; (defparameter *qqq* (make-instance 'dna-cube))
;; (write-oxdna *qqq* :filename "pflq")
;; (write-oxdna (make-instance 'dna-cone) :filename "cone")
;; (defparameter *t* (make-instance 'dna-triangle))
;; (defparameter *c* (make-instance 'dna-cone))

;; (length (connected-nts (5nt *t*)))
;; (length (remove nil (mapcar #'partner (connected-nts (5nt *t*)))))

;; (let* ((nts (connected-nts (5nt *t*)))
;;        (pts (mapcar #'partner (connected-nts (5nt *t*))))
;;        (sames (mapcar #'(lambda (x y)
;; 			  (if y
;; 			      (magicl::= (vn x) (scale (vn y) -1))
;; 			      t))
;; 		      nts pts)))
;;   (remove t sames))

;; (magicl::= (v3 -1 0 0) (scale (v3 1 0 0) -1))
;; (scale (v3 1 0 0) -1)


;; (length (connected-nts (5nt *c*)))

;; (length (remove nil (mapcar #'partner (connected-nts (5nt *c*)))))
