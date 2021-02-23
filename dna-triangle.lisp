(in-package :small)


(defclass/std dna-triangle (dna-origami)
  ((joining-strands :doc "list of joining strands")
   (internal-staps :doc "list of internal staple strands")
   (capping-staps :doc "list of capping staple strands")
   )
  (:documentation "An implementation the DNA a single triangle of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))








(defun s-staple-tri (tri i starts lengths)
  "creates an s-shaped staple strand to hold tri helices i, i+1 and i+2 together.
Starts are taken from tri edges"
  (let* ((hi (SMALL::find-obj-with-props (scaffold tri)
					  `((:i . ,i))))
	 (hi+1 (small::find-obj-with-props (scaffold tri)
					  `((:i . ,(+ i 1)))))
	 (hi+2 (small::find-obj-with-props (scaffold tri)
					   `((:i . ,(+ i 2)))))
	 (ends (mapcar #'+ starts lengths)))
    (if (evenp i) ;;TODO: Add error checking on i and k
	(SMALL::create-staple `((:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end nil)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end t)
				(:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end nil)))
	(SMALL::create-staple `((:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end t)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end nil)
				(:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end t))))))



(defmethod internal-staples ((obj dna-triangle))
  (let (staps)
    (push (loop
	    for i from 2 to 20 by 2
	    collect
	    ;; (s-staple-tri obj  i '(23 16 16) '(8 15 7))
	    (s-staple-tri obj  i '(24 16 16) '(7 15 7))
	    )
	  staps)
    (push (loop
	    for i from 5 to 18 by 2
	    collect
	    (s-staple-tri obj  i '(39 31 31) '(8 16 8)))
	  staps)
    (push (loop
	    for i from 6 to 16 by 2
	    collect
	    (s-staple-tri obj  i '(55 47 47) '(8 16 8)))
	  staps)    
    (push (loop
	    for i from 7 to 15 by 2
	    collect
	    (s-staple-tri obj  i '(70 63 63) '(8 15 7)))
	  staps)
    (push (loop
	    for i from 10 to 12 by 2
	    collect
	    (s-staple-tri obj  i '(86 78 78) '(8 16 8)))
	  staps)
    (push
     (s-staple-tri obj  11 '(102 94 94) '(8 16 8))
     staps)

     ;TODO: Maybe nreverse
    (reverse staps)))

     
(defun u-staple-tri (tri i1 s1 e1 f3e1  i2 s2 e2 f3e2 )
  "creates an u-shaped staple strand to hold tri helices i, i+1 together.
Starts are taken from tri edges"
  (let* ((h1 (SMALL::find-obj-with-props (scaffold tri)
					 `((:i . ,i1) )))
	 (h2 (small::find-obj-with-props (scaffold tri)
					 `((:i . ,i2))))
	 (stap (SMALL::create-staple `((:obj ,h1  :start ,s1 :end ,e1 :from-3end ,f3e1)
				       (:obj ,h2  :start ,s2 :end ,e2 :from-3end ,f3e2)))))
     stap))


    

(defun u-staples-tri (tri)
  (let* ((u1s ;; Staple u in row 1
	   (u-staple-tri tri   2 18 26 t  1 0 17 nil))
	 (u4s ;; Staple u in row 4	  
	  (u-staple-tri tri   4 13 29 t  5 30 38 nil))
	 (u9s ;; Staple u in row 9
	  (u-staple-tri tri   10 27 35 t  9 10 26 nil)))
    (list u4s u9s))) ;; Not returning u1 for now since we are use these positions for staples to join triangles together to form cones


(progn
  (defmethod initialize-instance :after ((ori dna-triangle) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (loop for i from 1 to 22 do
      (progn
	;;(break "scaff ~A" (scaffold ori))
	(add-to-scaffold ori (scaffold-helix 1 i))
	(when (evenp i)
	  (unless (= *2r* i)
	    (add-to-scaffold ori (SMALL::scaffold-loop 1 i)))
	  )))
					;(break ori)
    (mapcar #'(lambda (nt base)
		(with-accessors ((cm cm) (vbb vbb) (vn vn)) nt  
		  (update-base nt  base) ;Set the bases to match the m13 seq
		  ;; Update coords since we want regular carteisan
		  ;;and the paper defines y in the opposite direction
		  (setf (cm nt) (@ (from-diag '(1d0 -1d0 1d0)) cm)) 
		  (setf (vbb nt) (@ (from-diag '(1d0 -1d0 1d0)) vbb))
		  (setf (vn nt) (@ (from-diag '(1d0 -1d0 1d0)) vn))))
	    (connected-nts (5nt (first (scaffold ori))))				
	    (map 'list #'string  *m13mp18*))

    (setf (5nt ori) (5nt (find-obj-with-props (scaffold ori)
					      `((:i . 1) (:k . 1)))))
    (setf (3nt ori) (3nt (find-obj-with-props (scaffold ori)
					      `((:i . 22) (:k . 1)))))
    ;; Now we add  staples to hold this bad boy together. awwww yeah
    (setf (internal-staps ori) (internal-staples ori))
    (push (u-staples-tri ori) (internal-staps ori))
    (mapcar #'(lambda (stap)
		(add-parent stap ori))
	    (alexandria:flatten (internal-staps ori)))
    ;; These are cappping end added in a hacky way
    
    (setf (capping-staps ori) (capping-ends ori));; (capping-ends ori :indices '(1 3 5 7 9 11 13 15 17 19 21))
    (mapcar #'(lambda (stap)
		(add-parent stap ori))
	    (capping-staps ori))
	    
    ori)

  )
;; (write-oxdna (rotate-obj (make-instance 'dna-triangle) (rotation-matrix (v3 0 1 0) (/ pi 2))) :filename "tri2")
;; (break (all-tfms
;; 	(first (alexandria:flatten
;; 		(internal-staps
;; 		 (rotate-obj (make-instance 'dna-triangle) (rotation-matrix (v3 0 1 0) (/ pi 2))))))))


(defmethod write-oxdna ((obj dna-triangle) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  ;;(write-oxdna (5nt (first (scaffold obj))) :filename filename))  
  (wmdna filename (all-to-write obj)))


(defmethod all-to-write ((obj dna-triangle))
;  (break "~A" (all-tfms (first (first (internal-staps obj)))))
  (list
   (5nt obj)
   (joining-strands obj)
   (internal-staps obj)
   (capping-staps obj)
    ;; (mapcar #'5nt (alexandria:flatten (internal-staps obj)))
   ))

(defmethod connect ((o1 dna-triangle) (o2 dna-triangle) &rest rest)
  (dna-connect o1 o2)
  (connect (3nt o1) (5nt o2)))

(defmethod connect ((o1 dna-triangle) (o2 dna-single-strand) &rest rest)
  (dna-connect o1 o2)
  (connect (3nt o1) (5nt o2)))

(defmethod connect ((o1 dna-single-strand) (o2 dna-triangle) &rest rest)
  (dna-connect o1 o2)
  (connect (3nt o1) (5nt o2)))


(defun tri-edge (tri &key from22)
  "Returns a unit vector along the triangles edge
if from22=t then the vector will point from helix 22->21"
  (let ((nt1 (3nt (first (scaffold tri))))
	(nt2 (5nt (car (last (scaffold tri))))))
    (as-unit-vec (if from22
		     (nt1->nt2 nt2 nt1)
		     (nt1->nt2 nt1 nt2)))))

(defun edge->center (tri &key reverse)
  (let ((nt1 (3nt (first (scaffold tri))))
	(nt2 (5nt (first (scaffold tri)))))
    (as-unit-vec (if reverse
		     (nt1->nt2 nt2 nt1)
		     (nt1->nt2 nt1 nt2)))))



(defun join-triangle (t1 t2
		      &key (overlap-len 4)
			(indices '(1 5 9 13 17 21))
			parent)
  "Creates staple strands which connect triangle 1 and 2 with truncations on t2 and extensions on t1"
  (let* ((i1s indices)
	 (i2s (mapcar #'(lambda (x)
			  (- (+ *2r* 1) x))
		      i1s))
	 (staps (mapcar #'(lambda (i1 i2)
			    (triangle-joining-staples t1 i1 t2  i2
						      :overlap-len overlap-len))
			i1s i2s)))
    (mapcar #'(lambda (stap-pair i1 i2)
		(add-prop (first stap-pair) :i i1)
		(add-prop (first stap-pair) :join-strand t)
		(add-parent (first stap-pair)
			    (if parent
				parent
				t1))
		(push (first stap-pair) (joining-strands t1))		
		(add-prop (second stap-pair) :i i2)
		(add-prop (second stap-pair) :join-strand t)		
		(add-parent (second stap-pair) (if parent
				parent
				t2))
		(push (second stap-pair) (joining-strands t2)))	   
	    staps i1s i2s)
    staps))
			  

(defun capping-ends (triangle &key
			      (indices '(3 7 11 15 19))
			      (len 16)
				parent)
  (let* ((h1s (mapcar #'(lambda (x)
			  (find-obj-with-props
			   (scaffold triangle)
			   `((:i . ,x))))
			  indices))
	 (h2s (mapcar #'(lambda (x)
			  (find-obj-with-props
			   (scaffold triangle)
			   `((:i . ,(+ x 1)))))
			  indices))
	 (staps (mapcar
		 #'(lambda (h1 h2)
			 (create-staple `((:obj ,h1  :start 0 :end ,len  :from-3end t)
					  (:obj ,h2  :start 0 :end ,len  :from-3end nil))))

		 h1s h2s)))
    staps))


  


(defun triangle-joining-staples (t1 i1 t2 i2  &key (overlap-len 4))
  (let* ((h1-i1 (find-obj-with-props (scaffold t1)
				    `((:i . ,i1))))
	 (h1-i1+1 (find-obj-with-props (scaffold t1)
				      `((:i . ,(+ i1 1)) )))
	 (h2-i2 (find-obj-with-props (scaffold t2)
				     `((:i . ,i2) )))
	 (h2-i2-1 (find-obj-with-props (scaffold t2)
				       `((:i . ,(- i2 1)) )))
	 stap1 stap2)
    (multiple-value-bind (stap nts)
	(create-staple `((:obj ,h2-i2  :start 0 :end ,overlap-len  :from-3end nil)
			 (:obj ,h1-i1  :start 0 :end 16  :from-3end t)
			 (:obj ,h1-i1+1  :start 0 :end 16  :from-3end nil)))
      (setf stap1 stap))
    (multiple-value-bind (stap nts)
	(create-staple `((:obj ,h2-i2-1  :start 0 :end 16  :from-3end t)
			 (:obj ,h2-i2  :start ,overlap-len :end 16  :from-3end nil)))
      (setf stap2 stap))
					;(break "~A"  (list stap1 stap2))
    (list stap1 stap2)
    ))



(defmethod joining-strands-as-idt ((tri dna-triangle) prefix)
  (strands-as-idt prefix (joining-strands tri)))


