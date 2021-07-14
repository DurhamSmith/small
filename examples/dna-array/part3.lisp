(in-package :small)
(load "../part1.lisp")

(load "../part2.lisp")


(defun alternate (a b len)
  (loop for x from 1 to len collect
        (if (oddp x)
            a
            b)))


(defun staple-ordered-antiparallel-strands (strands i starts lengths &key from-3end desc)
  "Creates a staple strand to hold strands from together.
strands: List DNA strands that have property :i with value some numerical index. Subsequent indices should be antiparrallel alligned
i: First indice, subsequent are i+1 up to i+(length (-1 ) starts)
starts: list of nt offset
lengths: list how long that section of the staple should be
from-3end: if the first (:i i) start should be from the 5 end or 3 end
desc: indices traveresd i-..."
  (let* ((helices (if desc
                      (loop for x from i above (- i (length starts))
                            collect
                            (SMALL::find-obj-with-props strands
                                                        `((:i . ,x))))
                      (loop for x from i below (+ i (length starts))
                            collect
                            (SMALL::find-obj-with-props strands
                                                        `((:i . ,x))))))
         (ends (mapcar #'+ starts lengths))
         (f3ends (alternate (if from-3end
                                t
                                nil)
                            (if from-3end
                                nil
                                t)
                            (length starts)))
         (staple-spec (mapcar #'(lambda (hel start end f3e)
                                  (list :obj hel :start start :end end :from-3end f3e))
                              helices starts ends f3ends)))
    staple-spec))


(defmethod initialize-instance :after ((ori dna-triangle) &key)
  (loop for i from 1 to 22 do
    (progn
      ;;(break "scaff ~A" (scaffold ori))
      (add-to-scaffold ori (tri-scaffold-helix i))
      (when (evenp i)
        (unless (= *2r* i)
          (add-to-scaffold ori (tri-scaf-loop i)))
        )))
  ;; Now we set the 5' and 3' ends of the dna-tile
  (setf (5nt ori) (5nt (first (scaffold ori)))
        (3nt ori) (3nt (car (last (scaffold ori)))))
  ;; Lets add the internal staples holiding the triangle together
  (setf (staples ori)
        (alexandria:flatten
         (list
          (loop for i from 14 downto 12 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(77 77 85) '(8 16 8)
                  :desc t
                  :from-3end nil)))
          (loop for i from 7 upto 15 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(69 62 62) '(8 15 7)
                  :desc nil
                  :from-3end t)))
          (loop for i from 18 downto 8 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(46 46 54) '(8 16 8)
                  :desc t
                  :from-3end nil)))
          (loop for i from 5 upto 17 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(38 30 30) '(8 16 8)
                  :desc nil
                  :from-3end t)))
          (loop for i from 22 downto 4 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(15 15 22) '(8 15 8)  ;; zero based index
                  :desc t
                  :from-3end nil)))
          (create-staple
           (staple-ordered-antiparallel-strands
            (scaffold ori)
            11
            '(101 93 93) '(8 16 8)  ;; zero based index
            :desc nil
            :from-3end t)))))
  (mapcar #'(lambda (ss)
              (add-child ori ss))  ;; to get them to rotate together
          (staples ori))
  ori
)

(describe 'dna-origami)

(setf tri (make-instance 'dna-triangle))
(wmdna "./tri_int"
       (5nt (first (scaffold tri)))
       (staples tri))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 CONE                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-corner (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "Cone from triangle of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))



(defmethod initialize-instance :after ((obj dna-corner) &key)
  ;;Fist we loop over the scaffold so that we can set its sequence
  ;;This way when we make partners they have the correct seq
  (with-accessors ((t1 t1) (t2 t2) (t3 t3)) obj
    (add-child obj t1)
    (add-child obj t2)
    (add-child obj t3)
    (let* ((roty (rotation-matrix (v3 0 1 0) (/ pi -2))) ; - because we use normal coords
           rot2 rot3)

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
      (rotate-obj t3 rot3))))


(defun all-triangle (tri)
  (list
   (5nt (first (scaffold tri)))
   (staples tri)))


(defun all-corner (c)
  (list
   (all-triangle (t1 c))
   (all-triangle (t2 c))
   (all-triangle (t3 c))))

(describe 'dna-corner)


(wmdna "./corner" (all-corner (make-instance 'dna-corner)))



(defclass/std dna-cube (dna-origami)
  ((c1 :doc "cone 1" :std (make-instance 'dna-corner))
   (c2 :doc "cone 2" :std (make-instance 'dna-corner))
   (c3 :doc "cone 3" :std (make-instance 'dna-corner))
   (c4 :doc "cone 4" :std (make-instance 'dna-corner)))
  (:documentation "An implementation the DNA cube made from 12 triangles (4 cones) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))



(defun all-cube (cube)
  (list
   (all-corner (c1 cube))
   (all-corner (c2 cube))
   (all-corner (c3 cube))
   (all-corner (c4 cube))))


(defun align-cones (c1 c2 edge)
  ;;(break)
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



(defmethod initialize-instance :after ((ori dna-cube) &key)
  (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) ori
    (format t "pre ~A" (all-tfms c2))
    (align-cones c1 c2 1)
    (format t "post ~A" (all-tfms c2))
                                        ;   (align-cones c1 c3 2)
    (align-cones c1 c4 3))
  ori)

(t1 )


(wmdna "./cube" (all-cube (make-instance 'dna-cube)))

(make-instance 'dna-corner)
