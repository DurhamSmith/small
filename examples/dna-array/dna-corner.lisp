(in-package :small)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        DNA-CORNER STAPLE FNs        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun staple-bridges-corner (corner)
  (let ((staps (remove nil
                       (loop for k from 1 to 3 collect
                                               (remove nil
                                                       (loop for i from 1 to 22
                                                            collect
                                                            (staple-bridge-corner corner k i)))))))
    staps))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;     DNA-CORNER CLASS DEFINITION     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-corner (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "Corner from triangle of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

(defmethod corner-triangle ((corner dna-corner) num)
  (cond ((= num 1) (t1 corner))
	((= num 2) (t2 corner))
	((= num 3) (t3 corner))
	(t (error "Only [1,3] are valid indexes"))))

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
    (let ((staps (staple-bridges-corner obj)))
      (setf (staples obj) staps)
      ;(break staps)
      (mapcar #'(lambda (stap)
                  (add-child obj stap))
              staps)
      )

    
    ;; Set 3 and 5nt
    (setf (5nt obj) (5nt t1)
          (3nt obj) (3nt t3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;     DNA-CORNER WRITING FUNCTIONS    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cdr (all-triangle (make-instance 'dna-triangle)))
(defun all-corner (c)
  (list
   (5nt c)
   (cdr (all-triangle (t1 c)))
   (cdr (all-triangle (t2 c)))
   (cdr (all-triangle (t3 c)))
   (staples c)))

(wmdna "./corner" (all-corner (make-instance 'dna-corner)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       DNA-CORNER ALIGNING FNs       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
