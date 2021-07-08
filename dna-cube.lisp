(in-package :small)
(make-instance 'dna-origami)
(defclass/std dna-cube (dna-origami)
  ((c1 :doc "cone 1" :std (make-instance 'dna-cone))
   (c2 :doc "cone 2" :std (make-instance 'dna-cone))
   (c3 :doc "cone 3" :std (make-instance 'dna-cone))
   (c4 :doc "cone 4" :std (make-instance 'dna-cone)))
  
  (:documentation "An implementation the DNA cube made from 12 triangles (4 cones) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))

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

(progn
  (defmethod initialize-instance :after ((obj dna-cube) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) obj
      (align-cones c1 c2 1)
      (align-cones c1 c3 2)
      (align-cones c1 c4 3)
      (add-parent c1 obj)
      (add-parent c2 obj)
      (add-parent c3 obj)
      (add-parent c4 obj)
      (join-cube obj))
    (format t "~A" (tfms obj))
    ;(rotate-obj obj (rotation-matrix (v3 0 1 0) (/ pi 4)))
    obj
    )
;  (make-instance 'dna-cube)
     
  (defun join-cube (cube)
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
      (join-triangle (t1 c1) (t1 c2) :parent cube)
      (join-triangle (t2 c1) (t2 c3) :parent cube)
      (join-triangle (t3 c1) (t3 c4) :parent cube)
      (join-triangle (t3 c2) (t3 c3) :parent cube)
      (join-triangle (t2 c2) (t2 c4) :parent cube)
      (join-triangle (t1 c3) (t1 c4) :parent cube)
      
      ;(break  c1)
      ))
  )




;(break "~A" (length (stap-strands (c1 (make-instance 'dna-cube)))))
;(break (make-instance 'dna-cube))

  

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
     (all-to-write c1)
     (all-to-write c2)
     (all-to-write c3)
     (all-to-write c4)
     )))



(defmethod stap-bridges-as-idt ((cube dna-cube) prefix )
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
    (let ((name1 (concatenate 'string prefix "_stap_bridge_cone1"))
	  (name2 (concatenate 'string prefix "_stap_bridge_cone2"))
	  (name3 (concatenate 'string prefix "_stap_bridge_cone3"))
	  (name4 (concatenate 'string prefix "_stap_bridge_cone4")))
      (list
       (stap-bridges-as-idt c1 name1)
       (stap-bridges-as-idt c2 name2)
       (stap-bridges-as-idt c3 name3)
       (stap-bridges-as-idt c4 name4)))))

(defmethod joining-strands-as-idt ((cube dna-cube) prefix )
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
    (let ((name1 (concatenate 'string prefix "_joining_strand_cone1"))
	  (name2 (concatenate 'string prefix "_joining_strand_cone2"))
	  (name3 (concatenate 'string prefix "_joining_strand_cone3"))
	  (name4 (concatenate 'string prefix "_joining_strand_cone4")))
      (list
       (joining-strands-as-idt c1 name1)
       (joining-strands-as-idt c2 name2)
       (joining-strands-as-idt c3 name3)
       (joining-strands-as-idt c4 name4)))))

(defmethod internal-staps-as-idt ((cube dna-cube) prefix )
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
    (let ((name1 (concatenate 'string prefix "_internal_stap_cone1"))
	  (name2 (concatenate 'string prefix "_internal_stap_cone2"))
	  (name3 (concatenate 'string prefix "_internal_stap_cone3"))
	  (name4 (concatenate 'string prefix "_internal_stap_cone4")))
      (list
       (internal-staps-as-idt c1 name1)
       (internal-staps-as-idt c2 name2)
       (internal-staps-as-idt c3 name3)
       (internal-staps-as-idt c4 name4)))))

(defun as-idt (cube)
  (let* ((idt (list (stap-bridges-as-idt cube "cube")
		    (joining-strands-as-idt cube "cube")
		    (internal-staps-as-idt cube "cube"))))
    idt))
