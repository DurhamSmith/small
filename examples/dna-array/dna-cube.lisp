(in-package :small)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  DNA-CUBE STAPLES: Joining Triangle ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join-cube (cube &key (overlap-len 2))
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
      (join-triangle (t1 c1) (t1 c2) :parent cube :overlap-len overlap-len)
      (join-triangle (t2 c1) (t2 c3) :parent cube :overlap-len overlap-len)
      (join-triangle (t3 c1) (t3 c4) :parent cube :overlap-len overlap-len)
      (join-triangle (t3 c2) (t3 c3) :parent cube :overlap-len overlap-len)
      (join-triangle (t2 c2) (t2 c4) :parent cube :overlap-len overlap-len)
      (join-triangle (t1 c3) (t1 c4) :parent cube :overlap-len overlap-len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      DNA-CUBE CLASS DEFINITION      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-cube (dna-origami)
  ((c1 :doc "corner 1" :std (make-instance 'dna-corner))
   (c2 :doc "corner 2" :std (make-instance 'dna-corner))
   (c3 :doc "corner 3" :std (make-instance 'dna-corner))
   (c4 :doc "corner 4" :std (make-instance 'dna-corner)))
  (:documentation "An implementation the DNA cube made from 12 triangles (4 corners) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))

(defmethod initialize-instance :after ((ori dna-cube) &key)
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) ori
    (format t "pre ~A" (all-tfms c2))
    (align-corners c1 c2 1)
    (format t "post ~A" (all-tfms c2))
    (align-corners c1 c3 2)
    (align-corners c1 c4 3))
  (join-cube ori)

  ori)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      DNA-CONE WRITING FUNCTIONS     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-cube (cube)
  (list
   (all-corner (c1 cube))
   (all-corner (c2 cube))
   (all-corner (c3 cube))
   (all-corner (c4 cube))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               SCRATCH               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((c (make-instance 'dna-cube))
       ;(t1 (t1 c))
       ;(t2 (t2 c))
       ;; (staps (join-triangle t1 t2))
       ;; (staps (join-cube c))
       )
  ;staps
  (push (capping-ends (t1 (c1 c))) (staples (t1 (c1 c))))
  ;(push (capping-ends (t2 (c1 c))) (staples (t2 (c1 c))))
  ;(push (capping-ends (t3 (c1 c))) (staples (t3 (c1 c))))
  (wmdna "allc"
         (caar (all-cube c))
         ;staps
         ))
