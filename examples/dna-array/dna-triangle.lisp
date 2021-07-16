(ql:quickload :small)
(in-package :small)
(load "/home/dd/quicklisp/local-projects/small/examples/dna-array/utils.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              CONSTANTS              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *i1*  11.3d0
  "The length of the shortest double helix in the square DNA tile in nanometers")

(defparameter *r*  11
  "the total number of rows with increasing length in each of the four isosceles right triangles composing the square DNA tile")

(defparameter *2r*  (* 2 *r*)
  "The total number of rows in each of the four isosceles right triangles composing the square DNA tile")

(defparameter *g* 1.42d0 "Distance between the center of the square to the central vertex of each of the four triangles in nanometers")


(defparameter *w* (+ (* 2 (+ *i1* *g*))
		     (* (+ *helix-diameter* *helix-spacing*)
			(- (* 2 *r*)
			   1)))
  "The length of the side of the square DNA tile in nanometers")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      TRIANGLE COORDS FUNCTIONS      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ai (i)
  "Calculate the number of base pairs in the i-th of the a DNA triangle that makes up the DNA square"
  (if (or (> i *2r*) (< i 0))
      (error "~a is an invalid index for ai calculation. valid indices: [1, ~a]" i *2r*)
      (if (<= i *r*)
          (round (/ (+ *i1*
                       (* (+ *helix-diameter* *helix-spacing*)
                          (- i 1)))
                    *helix-nt-spacing*))
          (ai (+ *2r* 1 (- i))))))

(defun tri-ax-x (i)
  "Calculate the x coordinate of the helix axis of the ith helix in the first triangle that makes the DNA square"
  (float (+ (- (/ *w* 2))
            *i1*
            *g*
            (* (+ *helix-diameter* *helix-spacing*)
               (- i 1)))))

(defun tri-ax-y ()
  "Calculate the y coordinate of the helix axis of the helices in the first triangle that makes the DNA square"
  0d0)

(defun tri-ax-z (j)
  "Calculate the z coordinate of the helix axis of the jth base pair in the first triangle that makes the DNA square."
  (float (+ (- (/ *w* 2))
            (* *helix-nt-spacing* j))))

(defun tri-ax-coords (i j)
  "The coordinate location of the helix axis of the j th base pair in the i th row in the first triangle: C1,i,j =(cx, cy, cz)"
  (v3
   (tri-ax-x i)
   (tri-ax-y)
   (tri-ax-z j)))

(defun theta-1ij (i j &key (odd-offset 0) (even-offset 0))
  "The coordinates where scaffold base joins the backbone in the two-dimensional plane of the
j th base pair in the i th row in the first triangle."
  (let* ((rotation (mod (* (- j 1)
                           *rad/bp*)
                        (* 2 pi)))
         (theta (if (oddp i)
                    (mod (+ odd-offset rotation) (* 2 pi))
                    (mod (+ even-offset rotation) (* 2 pi)))))
    theta))

(defun theta-1ij-scaffold (i j)
  (theta-1ij i j
             :odd-offset 0
             :even-offset pi))


(defun tri-scaf-coords (i j &key cm)
  "The coordinate location of where the scaffold base joins the backbone in the two-dimensional plane of the j th base pair in the i th row in the first triangle"
  (let* ((helix-axis (tri-ax-coords i j))
         (theta (theta-1ij-scaffold i j))
         (cyl-vec (if cm
                      (v3 *helix-cm-offset* theta 0) ;; adjustment helix cm.
                      (v3 *helix-radius* theta 0)))     ; helix rad/bb cord = 1nm
         (cart-cyl (cylindrical->cartesian cyl-vec))
         (coords (.+ helix-axis cart-cyl)))
    coords))

(defun tri-scaffold-helix (i)
  (let* ((j (if (oddp i)
                (ai i)
                1))
         (5axis (if (oddp i)
                    (tri-ax-coords i j)
                    (tri-ax-coords i 1)))
         (3axis (if (oddp i)
                    (tri-ax-coords i 1)
                    (tri-ax-coords i (ai i))))
         (vn (as-unit-vec (.- 3axis 5axis)))
         (cm (tri-scaf-coords i j :cm t))
         (vbb0 (as-unit-vec (.- cm 5axis)))
         (hel (helix-strand 5axis vn vbb0 (ai i))))
    (add-prop hel :i i)
    hel))

(defun tri-scaf-loop (i)
  (let* ((c1 (tri-scaf-coords i (ai i)))
         (c2 (tri-scaf-coords (+ i 1) (ai (+ i 1))))
         (loop-strand (bridging-single-strand c1 c2 (v3 0 1 0))))
    (add-prop loop-strand :i i)
    loop-strand))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    DNA-TRIANGLE STAPLES: Internal   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; DNA-TRIANGLE STAPLES: Joining Triangles ;
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

(defun join-triangle (t1 t2
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    DNA-TRIANGLE STAPLES: Capping    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-capping-staple (tri i &key (len 16))
  "i = odd indexed triangle, caps i and i+1th helix "
  (create-staple
   (staple-ordered-antiparallel-strands (scaffold tri)
                                        i
                                        (list 0 0)
                                        (list len len)
                                        :desc nil
                                        :from-3end t)))

(defun cap-triangle-end (tri i &key (len 16))
  (let* ((cap-stap (create-capping-staple tri
                                          i
                                          :len len)))
    (add-prop cap-stap :i i)
    (add-prop cap-stap "capping-staple" t)
    (add-child tri cap-stap)
    cap-stap))


(defun cap-triangle (tri &key
                           (indices '(3 7 11 15 19))
                           (len 16))
  (mapcar #'(lambda (i)
              (cap-triangle-end tri i :len len))
          indices))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        DNA-TRIANGLE CLASS DEF       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass/std dna-triangle (dna-origami)
  ()
  (:documentation "An implementation the DNA a single triangle of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      DNA-TRIANGLE ALIGNMENT FNs     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      DNA-TILE WRITING FUNCTIONS     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-triangle (tri)
  (list
   (5nt (first (scaffold tri)))
   (children tri)
   ;; (staples tri)
   ))


(let* ((tri (make-instance 'dna-triangle)))
  (wmdna "uncapped" tri (children tri))
  (cap-triangle tri)
  (wmdna "capped" tri (children tri)))
