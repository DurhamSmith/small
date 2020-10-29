(in-package :small)

(defclass/std dna-cube (dna-origami)
  ((c1 :doc "cone 1" :std (make-instance 'dna-cone))
   (c2 :doc "cone 2" :std (make-instance 'dna-cone))
   (c3 :doc "cone 3" :std (make-instance 'dna-cone))
   (c4 :doc "cone 3" :std (make-instance 'dna-cone)))
  (:documentation "An implementation the DNA cube made from 12 triangles (4 cones) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))


(progn
  (defmethod initialize-instance :after ((obj dna-cube) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
      (let* ((r1 (rotation-matrix (v3 0 1 0) (/ pi 1))) ; - because we use normal coords
	     (r2 (rotation-matrix (v3 1 0 0 ) (/ pi 1)))
	     rot2 rot3 )
	;; Rotate first then from second rot mat
	(rotate-obj c2 r2)
;	(translate-obj c2 (v3 0 0 (- *w*)))
;	(break c2)
					;(rotate-obj t2 (rotation-matrix (v3 1 0 0) (/ pi 4)))
	
	obj)))
  (write-oxdna (make-instance 'dna-cube) :filename "ice-cube"))

(defmethod write-oxdna ((obj dna-cube) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
    ;(break "~A" (5nt c1))
    (wmdna filename
	   (5nt c1)
	   (5nt c2)
	   ;(first (scaffold t3))
	   )))
