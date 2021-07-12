(in-package :small)

(defclass/std CHEM-ATOM (CHEM-OBJ)
  ((xcoord :doc "x coord in nm")
   (ycoord :doc "y coord in nm")
   (zcoord :doc "z coord in nm")
   (coords :doc "z coord in nm")
   (element-type :std "Au")))

(defclass/std NP (CHEM-OBJ)
  ((xlen :doc "length in the x direction before any transformations")
   (ylen :doc "length in the y direction before any transformations")
   (zlen :doc "length in the z direction before any transformations")
   (xtrans)
   (ytrans)
   (ztrans))
  (:documentation "Nanoparticle class"))



 (defmethod initialize-instance :after ((np np) &key)
   "NP"
   (with-accessors ((xl xlen) (yl ylen) (zl zlen)
                    (xtrans xtrans) (ytrans ytrans) (ztrans ztrans))
       np
     (let* ((xmin (- (/ xl 2)))
            (xmax (/ xl 2))
            (ymin (- (/ xl 2)))
            (ymax (/ xl 2))
            (zmin (- (/ xl 2)))
            (zmax (/ xl 2))
            (atms (loop for x from xmin to xmax by xtrans collect
                                                          (progn
                                                            (loop for z
                                                                  from zmin to zmax
                                                                  by ztrans collect
                                                                            (list
                                                                             (make-instance 'chem-atom
                                                                                            :coords (v3 x ymin z))
                                                                             (make-instance 'chem-atom
                                                                                            :coords (v3 x ymax z))))
                                                            (loop for y
                                                                  from ymin to ymax
                                                                  by ytrans collect
                                                                            (list
                                                                             (make-instance 'chem-atom
                                                                                            :coords (v3 x y zmin))
                                                                             (make-instance 'chem-atom
                                                                                            :coords (v3 x y zmax))))))))
       ;; (loop for z from zmin to zmax by ztrans collect
       ;;       (progn
       ;;         (loop for z
       ;;               from zmin to zmax
       ;;               by ztrans collect
       ;;                         (list
       ;;                          (make-instance 'chem-atom
       ;;                                         :xcoord x
       ;;                                         :ycoord ymin
       ;;                                         :zcoord z)
       ;;                          (make-instance 'chem-atom
       ;;                                         :xcoord x
       ;;                                         :ycoord ymax
       ;;                                         :zcoord z)))
       ;;         ))
       ;;
       (write-list "./tst.xyz"
                   ;(append (list (format nil "~A" (length (flatten atms))))
                           (mapcar #'(lambda (a)
                                       (format nil "~A #\tab ~A #\tab    ~A    ~A" (element-type a) (x (coords a)) (y (coords a)) (z (coords a))))
                                   (flatten atms))))))

 (make-instance 'np
                :xlen 65.0
                :ylen 65.0
                :zlen 65.0
                :xtrans 0.28
                :ytrans 0.28
                :ztrans 0.28)
