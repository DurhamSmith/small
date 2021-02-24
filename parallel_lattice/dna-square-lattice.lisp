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
    (alexandria:flatten
     (loop for i from 0 to 7 collect
			     (loop for j from 0 to 7 collect
						     (make-lattice-helix i j)))))




(defun helix-i-j (scaff-list i j)
  (find-obj-with-props
   (alexandria::flatten scaff-list) `((:i . ,i) (:j . ,j))))



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

(defmethod initialize-instance :after ((ori dna-square-lattice) &key)
  (let ((*rad/bp* (deg->rad 33.75)))
    (setf (scaffold ori) (make-lattice-helices))))

  ;; (defun make-lattice-scaffold ()
  ;;   (let* ((fwd-coord0 (
  ;; 	 (fwd-axis (v3 0 1 0))
  ;; 	 (fwd-vbb (v3 -1 0 0))))
  
  
  
  
