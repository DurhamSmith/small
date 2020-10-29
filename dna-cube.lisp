(in-package :small)

(defclass/std dna-cube (dna-origami)
  ((c1 :doc "cone 1" :std (make-instance 'dna-cone))
   (c2 :doc "cone 2" :std (make-instance 'dna-cone))
   (c3 :doc "cone 3" :std (make-instance 'dna-cone))
   (c4 :doc "cone 4" :std (make-instance 'dna-cone)))
  
  (:documentation "An implementation the DNA cube made from 12 triangles (4 cones) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))


(progn
  (defmethod initialize-instance :after ((obj dna-cube) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
      (align-cones c1 c2 1)
      (align-cones c1 c3 2)
      (align-cones c1 c4 3))
    (join-cube obj)
    obj
    )
  (defun join-cube (cube)
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
    (join-triangle (t1 c1) (t1 c2))
    (join-triangle (t2 c1) (t2 c3))
    (join-triangle (t3 c1) (t3 c4))
    (join-triangle (t3 c2) (t3 c3))
    (join-triangle (t2 c2) (t2 c4))
    (join-triangle (t1 c3) (t1 c4))
    ))

  (write-oxdna (make-instance 'dna-cube) :filename "ice-cube"))


(defun align-cones (c1 c2 edge)
  (let* ((tri (cond ((= 1 edge) (t1 c1))
		    ((= 2 edge) (t2 c1))
		    ((= 3 edge) (t3 c1))
		    (t (error "invalid num for edge"))))
	 (tri2 (cond ((= 1 edge) (t1 c2))
		     ((= 2 edge) (t2 c2))
		     ((= 3 edge) (t3 c2))
		     (t (error "invalid num for edge"))))
	 (r1 (rotation-matrix
	      (tri-edge tri)
	      pi))
	 (r2 (rotation-matrix
	      (edge->center tri)
	      pi)))
    (rotate-obj c2 r1)
    (rotate-obj c2 r2)
    (translate-obj c2 (scale (edge->center tri)
			     (- *w*)))))

(nth 1 '(0 1 2 3))


  

(defmethod write-oxdna ((obj dna-cube) &key filename (all t) (start 0) (prev -1) (next -1) (strand 1))
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
    ;(break "~A" (5nt c1))
    (wmdna filename
	   (all-to-write obj)
	   )))

(defmethod all-to-write ((obj dna-cube))
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
    ;(break "~A" (5nt c1))
    (list
     (5nt c1)
     (joining-strands (t1 c1))
     (joining-strands (t2 c1))
     (joining-strands (t3 c1))
     (5nt c2)
     (joining-strands (t1 c2))
     (joining-strands (t2 c2))
     (joining-strands (t3 c2))
     (5nt c3)
     (joining-strands (t1 c3))
     (joining-strands (t2 c3))
     (joining-strands (t3 c3))
     (5nt c4)
     (joining-strands (t1 c4))
     (joining-strands (t2 c4))
     (joining-strands (t3 c4))

     
     )))



  (second (first (join-triangle
		  (make-instance 'dna-triangle) (make-instance 'dna-triangle) )))


  (break
   (join-triangle
    (make-instance 'dna-triangle) (make-instance 'dna-triangle) ))
  
