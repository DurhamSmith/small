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
(evenp 0)
(oddp 0)
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
    helix))

(wmdna "par"
       (loop for i from 0 to 7 collect
			       (loop for j from 0 to 7 collect
						       (make-lattice-helix i j))))
	 

(defun make-lattice-scaffold ()
  (let* ((fwd-coord0 (
	 (fwd-axis (v3 0 1 0))
	 (fwd-vbb (v3 -1 0 0))
	 
    
