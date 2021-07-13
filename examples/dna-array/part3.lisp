(in-package :small)


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
                            (SMALL::find-obj-with-props (scaffold tri)
                                                        `((:i . ,x))))
                      (loop for x from i below (+ i (length starts))
                            collect
                            (SMALL::find-obj-with-props (scaffold tri)
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


(setq tri (make-instance 'dna-triangle))
(wmdna "./tmp/tri_int"
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


(wmdna "./tmp/cube" (all-corner (make-instance 'dna-corner)))
