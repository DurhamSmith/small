(in-package :small)


(defclass/std dna-cone (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "An implementation the DNA a cone made from 3 triangles of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))



(defun staple-bridge-cone (cone k i)
  (let* ((prevk (if (= k 1)
		    3
		    (- k 1)))
	 (nextk (if (= k 3)
		    1
		    (+ k 1)))
	 (prev-tri (cone-triangle cone prevk))
	 (cur-tri (cone-triangle cone k))
	 (next-tri (cone-triangle cone nextk)))
    (cond ((= i 2)
	   (let* ((h1 (find-obj-with-props (scaffold prev-tri)
						  `((:i . ,(- *2r* i)))))
		  (h2 (find-obj-with-props (scaffold prev-tri)
						  `((:i . ,(+ (- *2r* i) 1)))))
		  (h3 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i))))
		  (h4 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,(+ i 1)))))
		  (stap (create-staple
			 `((:obj ,h1  :start 12 :end 20  :from-3end t)
			   (:obj ,h2  :start 0 :end 11  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h3  :start 0  :end 11 :from-3end t)
			   (:obj ,h4  :start 12  :end 20 :from-3end nil)))))
	     stap))
	  ((= i 3)
	   (let* ((h1 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (h2 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 20) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 12  :from-3end nil)
			   (:single-strand t :num-nts 4)
			   (:obj ,h2  :start 0 :end 12  :from-3end t)))))
	     stap))
	  ((= i 4)
	   (let* ((h1 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 19) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 21  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h2  :start 0 :end 13  :from-3end t)))))
	     stap))
	  ((= i 5)
	   (let* ((h1 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,(+ i 1)) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (h3 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 18) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 22 :end 30  :from-3end t)
			   (:obj ,h2  :start 0 :end 22  :from-3end nil)
			   (:single-strand t :num-nts 6)
			   (:obj ,h3  :start 0 :end 14  :from-3end t)))))
	     stap))
	  ((= i 6)
	   (let* ((h1 (find-obj-with-props (scaffold prev-tri)
						  `((:i . ,17) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (h3 (find-obj-with-props (scaffold cur-tri)
						  `((:i . 7) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 7  :from-3end nil)
			   (:single-strand t :num-nts 4)
			   (:obj ,h2  :start 0 :end 14  :from-3end t)
			   (:obj ,h3  :start 16 :end 23  :from-3end nil)))))
	     stap))
	  ((= i 7)
	   (let* ((h1 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,8) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,7) )))
		  (h3 (find-obj-with-props (scaffold prev-tri)
						  `((:i . ,16) )))
		  (h4 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 15) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 9 :end 17 :from-3end t)
			   (:obj ,h2  :start 0 :end 8 :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h3  :start 0 :end 8  :from-3end t)
			   (:obj ,h4  :start 9 :end 17  :from-3end nil)))))
	     stap))
	  ((= i 8)
	   (let* ((h1 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 15) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 9  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h2  :start 0 :end 9  :from-3end t)))))
	     stap))
	  ((= i 9)
	   (let* ((h1 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (h2 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 14) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 10  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h2  :start 0 :end 18  :from-3end t)))))
	     stap))
	  ((= i 10)
	   (let* ((h1 (find-obj-with-props (scaffold prev-tri)
						  `((:i . 13) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,i) )))
		  (h3 (find-obj-with-props (scaffold cur-tri)
						  `((:i . ,(+ i 1)) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 11  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h2  :start 0 :end 19  :from-3end t)
			   (:obj ,h3  :start 19 :end 27  :from-3end nil)))))
	     stap))
	  ((and (= i 11) (= k 1));(or (= k 1) (= k 3)))
	   (let* ((h1 (find-obj-with-props (scaffold next-tri)
					   `((:i . ,i) )))
		  (h2 (find-obj-with-props (scaffold cur-tri)
					   `((:i . ,12) )))
		  (h3 (find-obj-with-props (scaffold cur-tri)
					   `((:i . ,i) )))
		  (h4 (find-obj-with-props (scaffold prev-tri)
					   `((:i . 12) )))
		  (h5 (find-obj-with-props (scaffold prev-tri)
					   `((:i . 11) )))
		  (h6 (find-obj-with-props (scaffold next-tri)
					   `((:i . 12) )))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 11 :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h2  :start 0 :end 11 :from-3end t)
			   (:obj ,h3  :start 0 :end 11  :from-3end nil)
			   (:single-strand t :num-nts 7)
			   (:obj ,h4  :start 0 :end 11  :from-3end t)
			   (:obj ,h5  :start 0 :end 11  :from-3end nil)
			   (:single-strand t :num-nts 3)
			   (:obj ,h6  :start 0 :end 11  :from-3end t)))))
	     stap))
	  
	  (t nil))))


(defun staple-bridges-cone (cone)
  (let ((staps (remove nil
		       (loop for k from 1 to 3 collect
					       (remove nil
						       (loop for i from 1 to 22 collect
										(staple-bridge-cone cone k i)))))))
    staps))



(defmethod cone-triangle ((cone dna-cone) num)
  (cond ((= num 1) (t1 cone))
	((= num 2) (t2 cone))
	((= num 3) (t3 cone))
	(t (error "Only [1,3] are valid indexes"))))





(progn
  (defmethod initialize-instance :after ((obj dna-cone) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
      (let* ((roty (rotation-matrix (v3 0 1 0) (/ pi -2))) ; - because we use normal coords
	     rot2 rot3
	     scloop1 scloop2)
	;; Rotate first then from second rot mat
	(rotate-obj t2 roty)
					;(rotate-obj t2 (rotation-matrix (v3 1 0 0) (/ pi 4)))
	(setf rot2 (rotation-matrix (midpoint (3nt t1)
					      (5nt t2))
				    (/ pi 2)))
	
	(rotate-obj t2 rot2)
	(rotate-obj t3 roty)
	(rotate-obj t3 roty)
	(rotate-obj t3 rot2)
	(setf rot3 (rotation-matrix (midpoint (3nt t2)
					      (5nt t3))
				    (/ pi 2)))
	(rotate-obj t3 rot3)
	(setf scloop1 (bridging-single-strand (backbone (3nt t1))
					       (backbone (5nt t2))
					        (v3 0 1 1)
					       :len 4))
	(setf scloop2 (bridging-single-strand (backbone (3nt t2))
					       (backbone (5nt t3))
					       (v3 0 1 1)
					       :len 4))
	;(break "~A" (list scloop1 scloop2))
	;; (connect t1 t2)
	;; (connect t1 t2)
	;; (connect t2 t3)
	(connect t1 scloop1)
	(connect scloop1 t2)
	(connect t2 scloop2)
	(connect scloop2 t3)
	(setf (5nt obj) (5nt t1))
	(setf (3nt obj) (3nt t3))
	;; Add as children so transformations on higher order objects will be done
	(add-child obj t1)
	(add-child obj scloop1)
	(add-child obj t2)
	(add-child obj scloop2)
	(add-child obj t3)
	(setf (5nt obj) (5nt t1))
	(setf (3nt obj) (3nt t3))
	;; Create bridge staples
	(setf (stap-bridges obj) (staple-bridges-cone obj))
	;;Add as children so transformations on higher order objects will be done
	(mapcar #'(lambda (x)
		    (add-parent x obj))
		;; flatten since we have 3 sets of staples
		(alexandria:flatten (stap-bridges obj)))
	;(break "~A" (connected-nts (5nt obj)))
	(mapcar #'(lambda (nt base)
		    (update-base nt  base)) ;Set the bases to match the m13 seq
		    ;; Update coords since we want regular carteisan
		    ;;and the paper defines y in the opposite direction
		(connected-nts (5nt obj))				
		(map 'list #'string  *m13mp18*))
	
	obj)))
 ;(write-oxdna (make-instance 'dna-cone) :filename "ice-cream")
 )

;; (defmethod write-oxdna ((obj dna-cone) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
;;   (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
;;     (wmdna filename
;; 	   (first (scaffold t1))
;; 	   ;(first (scaffold t2))
;; 	   ;(first (scaffold t3))
;; 	   )))


(defmethod write-oxdna ((obj dna-cone) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
    (wmdna filename
	   (all-to-write obj)
	   ;(first (scaffold t1))
	   ;(stap-bridges obj)
	   ;(first (scaffold t2))
	   ;(first (scaffold t3))
	   )))







(defmethod all-to-write ((obj dna-cone))
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
					;(break "~A" (5nt c1))
;;         (break (joining-strands t1))
    (list
     (5nt obj)
     ;;staple-bridges
     (stap-bridges obj)
     ;;internal staples
     (internal-staps t1)
     (internal-staps t2)
     (internal-staps t3)
     ;;These join tiles??
     (joining-strands t1)
     (joining-strands t2)
     (joining-strands t3)
     )))

			    

(defmethod joining-strands-as-idt ((cone dna-cone) prefix )
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) cone
    (let ((name1 (concatenate 'string prefix "_t1"))
	  (name2 (concatenate 'string prefix "_t2"))
	  (name3 (concatenate 'string prefix "_t3")))
      (list
       (joining-strands-as-idt t1 name1)
       (joining-strands-as-idt t2 name2)
       (joining-strands-as-idt t3 name3)))))

     
(defmethod stap-bridges-as-idt ((cone dna-cone) prefix)
  (strands-as-idt prefix (stap-bridges cone)))


