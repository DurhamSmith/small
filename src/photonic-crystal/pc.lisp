(ql:quickload '("defclass-std"  "py4cl2" "small"))
(defpackage :pc (:use #:cl #:defclass-std #:py4cl2 #:small))
(in-package :pc)
(setf (config-var 'py4cl2:pycmd) "/home/dd/anaconda3/envs/meep/bin/python")
(defpymodule "meep" t)
(defpymodule "meep.mpb" t :lisp-package "MPB")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Global Variables          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm/a* (/ 130 1) "Conversion factor to convert nanometers to units of a used in meep. Set using set-meep-system-size")
(defparameter *a/nm* (/ 1 130) "Conversion factor to convert units of a used in meep to nanometers. Set using set-meep-system-size")

(defun set-meep-system-size (nm a)
  "Sets the global variables *nm/a* and *a/nm*"
  (setf *nm/a* (/ nm a)
        *a/nm* (/ a nm)))


(set-meep-system-size (+ (* (cos (/ pi 4)) small::*w* 2) 5) 1)

(defmethod nm->a (vec)
  (magicl::scale vec *a/nm*))

(defmethod a->nm (vec)
  (magicl::scale vec *nm/a*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Dielectric Class          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std meep-dielectric (chem-obj)
  ((eps :doc "epsilon")
   (center :std (v3 0 0 0)
           :doc "The center of the dielectric block in nm")
   (size :doc "The size of the dielectric block in nm"))
  (:documentation "A dielectric material used by the meep and mbp software packages"))


(defmethod as-meep-dielectric ((di meep-dielectric))
  "Creates a dielectric material in meep units"
  (with-accessors ((eps eps) (center center) (size size)) di
    (let ((asize (nm->a size)) ; We convert to units of a since thats what the meep package requires
          ;; Here we need to make sure that if any translation have been do them in units of a not nm
          (acenter (apply-tfms-in-units-of-a di (nm->a center))))
      (meep:block/class :size (list (x asize) (y asize) (z asize))
                        :center (list (x acenter) (y acenter) (z acenter))
                        :material (meep:medium/class :epsilon eps )))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        Photonic Crystal Class       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std photonic-crystal (chem-obj)
  ((cell-size :doc "size of the unit cell in nm"))
  (:documentation "A class that will perform the simulation of the TM and TE band gaps of a photonic crystal. Any children meep-dielectrics are used for the PC's unit cell. TODO Currently only a (1 1 1) cell (in meeps units of a) is supported. Add class slots and allow customization of the cell size."))

(defmethod get-dielectrics ((obj chem-obj))
  "Returns any childrren that are meep-dielectrics"
  (remove-if-not #'(lambda (child) (typep child 'meep-dielectric))
                 (small::children obj)))

(defmethod get-band-structure ((pc photonic-crystal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Photonic Cube            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here we import the actual DNA cube and an array of it
(load "/home/dd/small_demos/dna-array/utils.lisp")
(load "/home/dd/small_demos/dna-array/dna-triangle.lisp")
(load "/home/dd/small_demos/dna-array/dna-corner.lisp")
(load "/home/dd/small_demos/dna-array/dna-cube.lisp")
(load "/home/dd/small_demos/dna-array/dna-cube-array.lisp")
;; Here we just have a dummy cube. Sometimes this can be useful if we just want
;; to test size/material combinations and we don't need other info than band struct
(defclass/std hybrid-cube (meep-dielectric small::dna-cube)
  ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            DNA-CUBE-ARRAY           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std hybrid-cube-array (small::dna-cube-array photonic-crystal)
  ()
  (:documentation "An array of dielectic cubes"))

(setq harrs (make-instance 'hybrid-cube-array :cube-type 'hybrid-cube))
(setq harrz (make-instance 'hybrid-cube-array :cube-type 'hybrid-cube))

(small::all-array harr)
(small::write-as "oxdna" "hybrid-array" (small::all-array harr))
(small::write-as "oxdna" "hybrid-arrays" (small::all-array harrs))
(small::write-as "oxdna" "hybrid-arrayz" (small::all-array harrz))
(get-dielectrics harr)



(defmethod initialize-instance :after ((harr hybrid-cube-array) &key)
  "Now we need to set the children cubes to have their center and eps set"
  (mapcar #'(lambda (cube)
              (setf (size cube) (v3 (* (cos (/ pi 4)) small::*w*)
                                    (* (cos (/ pi 4)) small::*w*)
                                    (* (cos (/ pi 4)) small::*w*))))
          (small::children harr))
  (loop for cube in (small::children harr)
        for i from 1
        if (oddp i)
          do (setf (eps cube) (* 1.46 1.46))  ;; e_sio2 = n_sio2^2
        else do (setf (eps cube) (* 2.61 2.61)))  ;; e_tio2 = n_tio2^2
  (small::translate-obj harr (v3 (+ (* (cos (/ pi 4)) small::*w* -1) -2.5)
                                 (+ (* (cos (/ pi 4)) small::*w* -1) -2.5)
                                 (+ (* (cos (/ pi 4)) small::*w* -1) -2.5))))


(mapcar #'(lambda (d)
            (list (cons "center"
                        (list (pyslot-value (pyslot-value d 'center) 'x)
                              (pyslot-value (pyslot-value d 'center) 'y)
                              (pyslot-value (pyslot-value d 'center) 'z)))
                  (cons "size"
                        (list (pyslot-value (pyslot-value d 'size) 'x)
                              (pyslot-value (pyslot-value d 'size) 'y)
                              (pyslot-value (pyslot-value d 'size) 'z)))
                  (cons "eps"
                        (pyslot-value
                         (pyslot-value (pyslot-value d 'material) 'epsilon-diag)
                         'x))))
        (mapcar #'as-meep-dielectric
                (get-dielectrics harrz)))





;; (defclass/std hybrid-cube-array (dna-cube-array photonic-crystal)
;;   ((cube-type :doc "class identifier of the type of cube that should be instantiated"
;;               :std 'tst-cube)
;;    (numx :doc "Number of cubes in the x direction" :std 2)
;;    (numy :doc "Number of cubes in the y direction" :std 2)
;;    (numz :doc "Number of cubes in the z direction" :std 2)
;;    (tx :doc "Number of nanometers to translate in the x direction"
;;        :std (+ (* (cos (/ pi 4)) *w*) 2.5))
;;    (ty :doc "Number of nanometers to translate in the y direction"
;;        :std (+ (* (cos (/ pi 4)) *w*) 2.5))
;;    (tz :doc "Number of nanometers to translate in the z direction"
;;        :std (+ (* (cos (/ pi 4)) *w*) 2.5)))
;;   (:documentation "An array of dielectic cubes"))


;; (defmethod initialize-instance :after ((array cube-array) &key)
;;   (with-accessors ((nx numx) (ny numy) (nz numz)
;;                    (tx tx) (ty ty) (tz tz) (cube-type cube-type)) array
;;     (loop for ix from 0 below nx
;;           collect (loop for iy from 0 below ny
;;                         collect (loop for iz from 0 below nz
;;                                       collect (let ((cube (make-instance 'tst-cube))
;;                                                     (tvec (v3
;;                                                            (* ix tx)
;;                                                            (* iy ty)
;;                                                            (* iz tz))))
;;                                                 (small::translate-obj cube tvec)
;;                                                 (small::add-child array cube)
;;                                                 (small::add-prop cube :array-index (list ix iy iz)))))))
;;   array)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  Scaling transforms for units of a  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scale-tfms (tfms scale)
    (let* ((scaled-tfms (mapcar #'(lambda (tfm)
                                    (if (string-equal (car tfm) "translate")
                                        (cons "translate" (magicl:scale (cdr tfm) scale))
                                        tfm))
                                tfms) ))
      scaled-tfms))


(defmethod apply-tfms-scaled ((obj chem-obj) v scale)
  "Does all the transformations that have been applied to the object in the order they were applied"
  (let* ((tfms (scale-tfms (small::all-tfms obj) scale))
         (rl (reverse tfms))
         (res (reduce #'small::apply-transformation tfms :initial-value  v :from-end t)))
    res))

(defmethod apply-tfms-in-units-of-a ((obj chem-obj) v)
  (apply-tfms-scaled obj v *a/nm*))

;; (mapcar #'(lambda (child)
;;             ;(apply-tfms-in-units-of-a child (nm->a (center child))))
;;              (nm->a (size child)))
;;         (small::children (make-instance 'cube-array)))
