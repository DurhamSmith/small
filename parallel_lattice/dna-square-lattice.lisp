(in-package :small)

(defclass/std dna-square-lattice (dna-origami)
  ((m :doc "The number of layers (layers are stacked in the z direction"
      :std 8)
   (n :doc "The number of helices (successive helices added in the x direction)"
      :std 8)
   (d :doc "The number of base paired helical turns (helices grow in the y direction)"
      :std 9)
   (scaff-loop-len-front
    :doc "The number of nucleotides before the main parallel helices start, in front of the 5 end of helix 0"
    :std 8)
   (scaff-loop-len-back
    :doc "The number of nucleotides before the main parallel helices start, in front of the 5 end of helix 0"
    :std 8))
  (:documentation "An implementation of Multilayer DNA Origami Packed on a Square Lattice (DOI: https://doi.org/10.1021/ja906381y). Note coordinate system differs from theirs, see the slot specs m, n and d for information of how these coordinates are defined."))




;; Here we bind new NT values so we can use them in construction


(defun make-lattice-helix (i j)
  (let* ((num-nts (*  (/ 9 0.75) 8))
	 (vaxis (if (evenp (+ i j))
		    (v3 0 1 0)
		    (v3 0 -1 0)))
	 (vbb0 (if (evenp (+ i j)) ;; TODO check if this is correct
		   (v3 -1 0 0)
		   (v3 1 0 0)))
	 (x (* j (+ *helix-diameter* *helix-spacing*)))
	 (y (if (evenp (+ i j))
		0
		(* (- num-nts 1) ; subtract 1 so that helices are correctly aligned
		   *helix-nt-spacing*)))
	 (z (* -1.0 i (+ *helix-diameter* *helix-spacing*)))
	 (coords0 (v3 x y z))
	 (helix (helix-strand coords0 vaxis vbb0 num-nts)))
    (add-prop helix :i i)
    (add-prop helix :j j)
    helix))


(defun make-lattice-helices ()
  (loop for i from 0 to 7 
	append (loop for j from 0 to 7
		     collect	(make-lattice-helix i j))))


(defun helix-i-j (scaff-list i j)
  (find-obj-with-props
   (alexandria::flatten scaff-list) `((:i . ,i) (:j . ,j))))


;; (defun crossover-plane-1 (ori)
;;   (let* ((is '(0 2 4 6))
;; 	 (js '(0 2 4 6))
;; 	 (scaf (scaffold ori))
;; 	 (staps (mapcar #'(lambda (i j)
;; 			    (let* ((h0 (helix-i-j scaf i j))
;; 				   (h1 (helix-i-j scaf i (1+ j)))
;; 				   (h2 (helix-i-j scaf (1+ i) (1+ j)))
;; 				   (stap (create-staple `((:obj ,h0 :start 0 :end 8 :from-3end nil)
;; 							  (:obj ,h1 :start 0 :end 8 :from-3end t)
;; 							  (:obj ,h2 :start 0 :end 8 :from-3end nil)))))
;; 			      stap))
;; 			is js)))
;;     staps))

(defun crossover-plane-1 (ori)
  (loop for i from 0 to 6 by 2
	append (loop for j from 0 to 6 by 2
		     collect
		     (cond ((and (= i 0) (evenp j))
			    (let* ((scaf (scaffold ori))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (h2 (helix-i-j scaf (1+ i) (1+ j)))
				   (stap (create-staple `((:obj ,h0 :start 0 :end 8 :from-3end nil)
							  (:obj ,h1 :start 0 :end 8 :from-3end t)
							  (:obj ,h2 :start 0 :end 8 :from-3end nil)))))
			      stap))
			   ((and (> i 0) (< i 6) (evenp j))
			    (let* ((scaf (scaffold ori))
				   (h-1 (helix-i-j scaf (1- i) j))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (h2 (helix-i-j scaf (1+ i) (1+ j)))
				   (stap (create-staple `((:obj ,h-1 :start 0 :end 8 :from-3end t)
							  (:obj ,h0 :start 0 :end 8 :from-3end nil)
							  (:obj ,h1 :start 0 :end 8 :from-3end t)
							  (:obj ,h2 :start 0 :end 8 :from-3end nil)))))
			      stap))
			   ((or
			     (and (> i 0) (< i 6))
			     (and (= i 6) (= j 6)))
			    (let* ((scaf (scaffold ori))
				   (h-1 (helix-i-j scaf (1- i) j))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (h2 (helix-i-j scaf (1+ i) (1+ j)))
				   (stap (create-staple `((:obj ,h-1 :start 0 :end 8 :from-3end t)
							  (:obj ,h0 :start 0 :end 8 :from-3end nil)
							  (:obj ,h1 :start 0 :end 8 :from-3end t)
							  (:obj ,h2 :start 0 :end 8 :from-3end nil)))))
			      stap))
			   ((and (= i 6) (/= j 6) (evenp j))
			    (let* ((scaf (scaffold ori))
				   (h-1 (helix-i-j scaf (1- i) j))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (h2 (helix-i-j scaf (1+ i) (1+ j)))
				   (h3 (helix-i-j scaf (1+ i) (+ j 2)))
				   (stap (create-staple `((:obj ,h-1 :start 0 :end 8 :from-3end t)
							  (:obj ,h0 :start 0 :end 8 :from-3end nil)
							  (:obj ,h1 :start 0 :end 8 :from-3end t)
							  (:obj ,h2 :start 0 :end 8 :from-3end nil)
							  (:obj ,h3 :start 0 :end 8 :from-3end t)))))
			      stap))
			   (t (error "Indexs out of bounds")))
		     )))


(defun crossover-plane-2 (ori)
  (loop for i from 0 to 7 by 1
	append (loop for j from 0 to 7 by 1
		     collect
		     (cond ((and (= i 0) (= j 1) (oddp j))
			    (let* ((scaf (scaffold ori))
				   (h-3 (helix-i-j scaf (+ i 2) (1- j)))
				   (h-2 (helix-i-j scaf (1+ i) (1- j)))
				   (h-1 (helix-i-j scaf (1+ i) j))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (stap (create-staple `((:obj ,h-3 :start 8 :end 16 :from-3end nil)
							  (:obj ,h-2 :start 8 :end 16 :from-3end t)
							  (:obj ,h-1 :start 8 :end 16 :from-3end nil)
							  (:obj ,h0 :start 8 :end 16 :from-3end t)
							  (:obj ,h1 :start 8 :end 16 :from-3end nil)))))
			      stap))
			   ((and (= i 0) (/= j 1) (/= j 7) (oddp j))
			    (let* ((scaf (scaffold ori))
				   (h-1 (helix-i-j scaf (1+ i) j))
				   (h0 (helix-i-j scaf i j))
				   (h1 (helix-i-j scaf i (1+ j)))
				   (stap (create-staple `((:obj ,h-1 :start 8 :end 16 :from-3end nil)
							  (:obj ,h0 :start 8 :end 16 :from-3end t)
							  (:obj ,h1 :start 8 :end 16 :from-3end nil)))))
			      stap))
			   ((and (= i 0) (= j 7) (oddp j))
			    (let* ((scaf (scaffold ori))
				   (h-5 (helix-i-j scaf (+ i 3) (- j 2)))
				   (h-4 (helix-i-j scaf (+ i 2) (- j 2)))
				   (h-3 (helix-i-j scaf (+ i 2) (- j 1)))
				   (h-2 (helix-i-j scaf (+ i 1) (- j 1)))
				   (h-1 (helix-i-j scaf (1+ i) j))
				   (h0 (helix-i-j scaf i j))				   
				   (stap (create-staple `((:obj ,h-4 :start 8 :end 16 :from-3end t)
							  (:obj ,h-3 :start 8 :end 16 :from-3end nil)
							  (:obj ,h-2 :start 8 :end 16 :from-3end t)
							  (:obj ,h-1 :start 8 :end 16 :from-3end nil)
							  (:obj ,h0 :start 8 :end 24 :from-3end t)))))
			      stap))
			   ;; ((and (= i 1) (evenp j))
			   ;;  (let* ((scaf (scaffold ori))
			   ;; 	   (h0 (helix-i-j scaf i j))
			   ;; 	   (h1 (helix-i-j scaf (+ i 1) j))
			   ;; 	   (h2 (helix-i-j scaf (+ i 1) (+ j 1)))
			   ;; 	   (h3 (helix-i-j scaf (+ i 2) (+ j 1)))				   
			   ;; 	   (stap (create-staple `((:obj ,h0 :start 8 :end 16 :from-3end t)
			   ;; 				  (:obj ,h1 :start 8 :end 16 :from-3end nil)
			   ;; 				  (:obj ,h2 :start 8 :end 16 :from-3end t)
			   ;; 				  (:obj ,h3 :start 8 :end 16 :from-3end nil)))))
			   ;;    stap))
			   (t nil)))))

(wmdna "cp-2" ;(scaffold *pl*)
       (remove nil (crossover-plane-1 *pl*))
       (remove nil (crossover-plane-2 *pl*)))

(cond (case1 (do-this))
      (case2 (do-this))
      (t default))





(defun blah (ori)
  (let* ((scaf (scaffold ori))
	 (h0 (helix-i-j scaf 0 0))
	 (h1 (helix-i-j scaf 0 1))
	 (h2 (helix-i-j scaf 1 1))
	 (stap (create-staple `((:obj ,h0 :start 0 :end 8 :from-3end nil)
				(:obj ,h1 :start 0 :end 8 :from-3end t)
				(:obj ,h2 :start 0 :end 8 :from-3end nil)))))
    stap))





(wmdna "lat"
       (helix-i-j (scaffold *pl*) 0 0)
       (helix-i-j (scaffold *pl*) 0 1)
       (helix-i-j (scaffold *pl*) 1 1)
       (blah *pl*))


(defparameter *pl* (make-instance 'dna-square-lattice))
(scaff

 (defmethod initialize-instance :after ((ori dna-square-lattice) &key)
   (let ((*rad/bp* (deg->rad 33.75)))
     (setf (scaffold ori) (make-lattice-helices))))

 ;; (defun make-lattice-scaffold ()
 ;;   (let* ((fwd-coord0 (
 ;; 	 (fwd-axis (v3 0 1 0))
 ;; 	 (fwd-vbb (v3 -1 0 0))))
 
 
 
 
