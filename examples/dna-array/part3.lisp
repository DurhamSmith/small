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
                  '(78 78 86) '(8 16 8)
                  :desc t
                  :from-3end nil)))
          (loop for i from 7 upto 15 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(70 63 63) '(8 15 7)
                  :desc nil
                  :from-3end t)))
          (loop for i from 18 downto 8 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(47 47 55) '(8 16 8)
                  :desc t
                  :from-3end nil)))
          (loop for i from 5 upto 17 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(39 31 31) '(8 16 8)
                  :desc nil
                  :from-3end t)))
          (loop for i from 22 downto 4 by 2
                collect
                (create-staple
                 (staple-ordered-antiparallel-strands
                  (scaffold ori)
                  i
                  '(16 16 23) '(7 15 8)  ;; 23 and 7 since there is a crossover. NOT ?zero based index
                  :desc t
                  :from-3end nil)))
          (create-staple
           (staple-ordered-antiparallel-strands
            (scaffold ori)
            11
            '(102 94 94) '(8 16 8)  ;; zero based index
            :desc nil
            :from-3end t)))))
  (mapcar #'(lambda (ss)
              (add-child ori ss))  ;; to get them to rotate together
          (staples ori))
  ori
)

(describe 'dna-origami)

(let ((tri (make-instance 'dna-triangle)))
  (wmdna "./tri_int"
         (5nt (first (scaffold tri)))
         (staples tri)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 CORNER                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-corner (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "Corner from triangle of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))



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
      (rotate-obj t3 rot3))
    (connect t1 t2) ;; Make them one origami
    (connect t2 t3)
    ;; Add staples
    (setf (staples obj)
        (staple-bridges-corner obj)

    )))


(defun all-triangle (tri)
  (list
   (5nt (first (scaffold tri)))
   (staples tri)))


(defun all-corner (c)
  (list
   (all-triangle (t1 c))
   (all-triangle (t2 c))
   (all-triangle (t3 c))
   (staples c)))

(describe 'dna-corner)


(wmdna "./corner" (all-corner (make-instance 'dna-corner)))




(defclass/std dna-cube (dna-origami)
  ((c1 :doc "corner 1" :std (make-instance 'dna-corner))
   (c2 :doc "corner 2" :std (make-instance 'dna-corner))
   (c3 :doc "corner 3" :std (make-instance 'dna-corner))
   (c4 :doc "corner 4" :std (make-instance 'dna-corner)))
  (:documentation "An implementation the DNA cube made from 12 triangles (4 corners) of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))



(defun all-cube (cube)
  (list
   (all-corner (c1 cube))
   (all-corner (c2 cube))
   (all-corner (c3 cube))
   (all-corner (c4 cube))))


(defun align-corners (c1 c2 edge)
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
    (align-corners c1 c2 1)
    (format t "post ~A" (all-tfms c2))
    (align-corners c1 c3 2)
    (align-corners c1 c4 3))
  ori)




(wmdna "./cube" (all-cube (make-instance 'dna-cube)))

(make-instance 'dna-corner)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        staple bridges to corner       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun staple-bridge-corner (corner k i)
  (let* ((prevk (if (= k 1)
                    3
                    (- k 1)))
         (nextk (if (= k 3)
                    1
                    (+ k 1)))
         (prev-tri (corner-triangle corner prevk))
         (cur-tri (corner-triangle corner k))
         (next-tri (corner-triangle corner nextk)))
    (cond ((= i 2)
           (let* ((h1 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . ,(- *2r* i)))))
                  (h2 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . ,(+ (- *2r* i) 1)))))
                  (h3 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i))))
                  (h4 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,(+ i 1)))))
                  (stap (create-staple
                         `((:obj ,h1  :start 12 :end 20  :from-3end t)
                           (:obj ,h2  :start 0 :end 11  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h3  :start 0  :end 11 :from-3end t)
                           (:obj ,h4  :start 12  :end 20 :from-3end nil)))))
             stap))
          ((= i 3)
           (let* ((h1 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h2 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 20) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 12  :from-3end nil)
                           (:single-strand t :num-nts 4)
                           (:obj ,h2  :start 0 :end 12  :from-3end t)))))
             stap))
          ((= i 4)
           (let* ((h1 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 19) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 21  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h2  :start 0 :end 13  :from-3end t)))))
             stap))
          ((= i 5)
           (let* ((h1 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,(+ i 1)) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h3 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 18) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 22 :end 30  :from-3end t)
                           (:obj ,h2  :start 0 :end 22  :from-3end nil)
                           (:single-strand t :num-nts 6)
                           (:obj ,h3  :start 0 :end 14  :from-3end t)))))
             stap))
          ((= i 6)
           (let* ((h1 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . ,17) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h3 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . 7) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 7  :from-3end nil)
                           (:single-strand t :num-nts 4)
                           (:obj ,h2  :start 0 :end 14  :from-3end t)
                           (:obj ,h3  :start 16 :end 23  :from-3end nil)))))
             stap))
          ((= i 7)
           (let* ((h1 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,8) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,7) )))
                  (h3 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . ,16) )))
                  (h4 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 15) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 9 :end 17 :from-3end t)
                           (:obj ,h2  :start 0 :end 8 :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h3  :start 0 :end 8  :from-3end t)
                           (:obj ,h4  :start 9 :end 17  :from-3end nil)))))
             stap))
          ((= i 8)
           (let* ((h1 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 15) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 9  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h2  :start 0 :end 9  :from-3end t)))))
             stap))
          ((= i 9)
           (let* ((h1 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h2 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 14) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 10  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h2  :start 0 :end 18  :from-3end t)))))
             stap))
          ((= i 10)
           (let* ((h1 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 13) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h3 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,(+ i 1)) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 11  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h2  :start 0 :end 19  :from-3end t)
                           (:obj ,h3  :start 19 :end 27  :from-3end nil)))))
             stap))
          ((and (= i 11) (= k 1));(or (= k 1) (= k 3)))
           (let* ((h1 (find-obj-with-props (scaffold next-tri)
                                           `((:i . ,i) )))
                  (h2 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,12) )))
                  (h3 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . ,i) )))
                  (h4 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 12) )))
                  (h5 (find-obj-with-props (scaffold prev-tri)
                                           `((:i . 11) )))
                  (h6 (find-obj-with-props (scaffold next-tri)
                                           `((:i . 12) )))
                  (stap (create-staple
                         `((:obj ,h1  :start 0 :end 11 :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h2  :start 0 :end 11 :from-3end t)
                           (:obj ,h3  :start 0 :end 11  :from-3end nil)
                           (:single-strand t :num-nts 7)
                           (:obj ,h4  :start 0 :end 11  :from-3end t)
                           (:obj ,h5  :start 0 :end 11  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h6  :start 0 :end 11  :from-3end t)))))
             stap))
          ((= i 22) ;; These hold the corners edges together BUT MIGHT BE WRONG (5->3) directions
           (let* ((h1 (find-obj-with-props (scaffold cur-tri)
                                           `((:i . 22) )))
                  (h2 (find-obj-with-props (scaffold next-tri)
                                           `((:i . 1) )))
                  (h3 (find-obj-with-props (scaffold next-tri)
                                           `((:i . 2) )))
                  (stap (create-staple
                         `((:obj ,h3  :start 18 :end 26  :from-3end t)
                           (:obj ,h2  :start 0 :end 17  :from-3end nil)
                           (:single-strand t :num-nts 3)
                           (:obj ,h1  :start 23 :end 33  :from-3end nil)))))
             stap))
          (t nil))))


(defun staple-bridges-corner (corner)
  (let ((staps (remove nil
		       (loop for k from 1 to 3 collect
					       (remove nil
						       (loop for i from 1 to 22 collect
										(staple-bridge-corner corner k i)))))))
    staps))



(defmethod corner-triangle ((corner dna-corner) num)
  (cond ((= num 1) (t1 corner))
	((= num 2) (t2 corner))
	((= num 3) (t3 corner))
	(t (error "Only [1,3] are valid indexes"))))


(let ((corner (make-instance 'dna-corner)))
  (wmdna "corner_stap_briges_internal_staps_1"
         (car (all-corner corner))
         (staple-bridges-corner corner)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Triangle Joining          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun triangle-joining-staples (t1 i1 t2 i2  &key (overlap-len 4))
  "Joins triangle t1 helice i1 to t2 i2,
Returns values staple1 (ext) staple2 (tru)"
  (let* ((h1-i1 (find-obj-with-props (scaffold t1)
                                     `((:i . ,i1))))
         (h1-i1+1 (find-obj-with-props (scaffold t1)
                                       `((:i . ,(+ i1 1)) )))
         (h2-i2 (find-obj-with-props (scaffold t2)
                                     `((:i . ,i2) )))
         (h2-i2-1 (find-obj-with-props (scaffold t2)
                                       `((:i . ,(- i2 1)) )))
         stap1 stap2)
    (multiple-value-bind (stap nts)
        (create-staple `((:obj ,h2-i2  :start 0 :end ,overlap-len  :from-3end nil)
                         (:obj ,h1-i1  :start 0 :end 16  :from-3end t)
                         (:obj ,h1-i1+1  :start 0 :end 16  :from-3end nil)))
      (setf stap1 stap))
    (multiple-value-bind (stap nts)
        (create-staple `((:obj ,h2-i2-1  :start 0 :end 16  :from-3end t)
                         (:obj ,h2-i2  :start ,overlap-len :end 16  :from-3end nil)))
      (setf stap2 stap))
                                        ;(break "~A"  (list stap1 stap2))
    (list stap1 stap2)
    ))


(defun join-triangles (t1 t2
                       &key (overlap-len 4)
                         (indices '(1 5 9 13 17 21))
                         parent)
  "Creates staple strands which connect triangle 1 and 2 with truncations on t2 and extensions on t1"
  (let* ((i1s indices)
         (i2s (mapcar #'(lambda (x)
                          (- (+ *2r* 1) x))
                      i1s))
         (staps (mapcar #'(lambda (i1 i2)
                            (triangle-joining-staples t1 i1 t2  i2
                                                      :overlap-len overlap-len))
                        i1s i2s)))
    (mapcar #'(lambda (stap-pair i1 i2)
                (add-prop (first stap-pair) :i i1)
                (add-prop (first stap-pair) :join-strand t)
                (add-parent (first stap-pair)
                            (if parent
                                parent
                                t1))
                (push (first stap-pair) (staples t1))
                (add-prop (second stap-pair) :i i2)
                (add-prop (second stap-pair) :join-strand t)
                (add-parent (second stap-pair) (if parent
                                                   parent
                                                   t2))
                (push (second stap-pair) (staples t2)))
            staps i1s i2s)
    staps))




(defun join-cube (cube &key (overlap-len 2))
    (with-accessors ((c1 c1) (c2 c2) (c3 c3) (c4 c4)) cube
      (join-triangle (t1 c1) (t1 c2) :parent cube :overlap-len overlap-len)
      (join-triangle (t2 c1) (t2 c3) :parent cube :overlap-len overlap-len)
      (join-triangle (t3 c1) (t3 c4) :parent cube :overlap-len overlap-len)
      (join-triangle (t3 c2) (t3 c3) :parent cube :overlap-len overlap-len)
      (join-triangle (t2 c2) (t2 c4) :parent cube :overlap-len overlap-len)
      (join-triangle (t1 c3) (t1 c4) :parent cube :overlap-len overlap-len)

                                        ;(break  c1)
      ))



(defun capping-ends (triangle &key
                                (indices '(3 7 11 15 19))
                                (len 16)
                                parent)
  (let* ((h1s (mapcar #'(lambda (x)
                          (find-obj-with-props
                           (scaffold triangle)
                           `((:i . ,x))))
                      indices))
         (h2s (mapcar #'(lambda (x)
                          (find-obj-with-props
                           (scaffold triangle)
                           `((:i . ,(+ x 1)))))
                      indices))
         (staps (mapcar
                 #'(lambda (h1 h2)
                     (create-staple `((:obj ,h1  :start 0 :end ,len  :from-3end t)
                                      (:obj ,h2  :start 0 :end ,len  :from-3end nil))))

                 h1s h2s)))
    (mapcar #'(lambda (stap)
                (add-child triangle stap))
            staps)

    staps))

(let* ((c (make-instance 'dna-cube))
       ;(t1 (t1 c))
       ;(t2 (t2 c))
       ;; (staps (join-triangle t1 t2))
       ;; (staps (join-cube c))
       )
  ;staps
  (push (capping-ends (t1 (c1 c))) (staples (t1 (c1 c))))
  (wmdna "joining"
         (all-triangle (t1 (c1 c)))
         ;staps
         ))
