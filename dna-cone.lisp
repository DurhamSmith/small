(in-package :small)


(defclass/std dna-cone (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
  (:documentation "An implementation the DNA a cone made from 3 triangles of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords")))


(progn
 (defmethod initialize-instance :after ((obj dna-cone) &key)  
  ;;Fist we loop over the scaffold so that we can set its sequence
  ;;This way when we make partners they have the correct seq
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
    (let* ((roty (rotation-matrix (v3 0 1 0) (/ pi -2))) ; - because we use normal coords
	   rot2 rot3 )
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
      (connect t1 t2)
      (connect t2 t3)
      (setf (5nt obj) (5nt t1))
      (setf (3nt obj) (3nt t3))
      ;; Add as children so transformations on higher order objects will be done
      (add-child obj t1)
      (add-child obj t2)
      (add-child obj t3)
      obj)))
 (write-oxdna (make-instance 'dna-cone) :filename "ice-cream"))

(defmethod write-oxdna ((obj dna-cone) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
    (wmdna filename
	   (first (scaffold t1))
	   ;(first (scaffold t2))
	   ;(first (scaffold t3))
	   )))






