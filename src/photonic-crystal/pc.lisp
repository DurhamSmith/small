;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Package Importing and loading the DNA cube array definitions       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload :small)
(ql:quickload '("defclass-std"  "py4cl2"))
(defpackage :small (:use #:cl #:defclass-std #:py4cl2 #:small))
(in-package :small)

;; Here we import the actual DNA cube and an array of it
(load "/home/dd/small_demos/dna-array/utils.lisp")
(load "/home/dd/small_demos/dna-array/dna-triangle.lisp")
(load "/home/dd/small_demos/dna-array/dna-corner.lisp")
(load "/home/dd/small_demos/dna-array/dna-cube.lisp")
(load "/home/dd/small_demos/dna-array/dna-cube-array.lisp")

;; Here we just have a dummy cube. Sometimes this can be useful if we just want
;; to test size/material combinations and we don't need other info than band struct

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             py4cl2 setup            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf (py4cl2::config-var 'py4cl2:pycmd) "/home/dd/anaconda3/envs/meep/bin/python")

(py4cl2:defpymodule "meep" t)
(py4cl2:defpymodule "meep.mpb" t :lisp-package "MPB")
(py4cl2:defpymodule "matplotlib.pyplot" nil :lisp-package "PLT")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Global Variables          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nm/a* (/ 130 1) "Conversion factor to convert nanometers to units of a used in meep. Set using set-meep-system-size")
(defparameter *a/nm* (/ 1 130) "Conversion factor to convert units of a used in meep to nanometers. Set using set-meep-system-size")

(defmethod nm->a (vec)
  "Converts a measurment in nanometers to MEEP/MPBs units of a"
  (magicl::scale vec *a/nm*))

(defmethod a->nm (vec)
  "Converts a measurment in MEEP/MPBs units of a to nanometers"
  (magicl::scale vec *nm/a*))

(defun set-meep-system-size (nm a)
  "Sets the global variables *nm/a* and *a/nm*"
  (setf *nm/a* (/ nm a)
        *a/nm* (/ a nm)))


(defparameter *w* 88.44) ;; Width of the side of a DNA-Triangle
;; We multiply because triangle edges run between opposite corners of the cubes face
;; This gives us the side length of the cube
(set-meep-system-size (+ (* (cos (/ pi 4)) *w* 2) 5) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;  Scaling transforms for units of a  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scale-tfms (tfms scale)
  "Takes a list of valid transformation in small and scales them to meeps units of a.
NOTE only translations are scaled since rotations are invariant when shrinking/expanding coordinate system "
    (let* ((scaled-tfms (mapcar #'(lambda (tfm)
                                    (if (string-equal (car tfm) "translate")
                                        (cons "translate" (magicl:scale (cdr tfm) scale))
                                        tfm))
                                tfms) ))
      scaled-tfms))


(defmethod apply-tfms-scaled ((obj chem-obj) v scale)
  "First scales translations then does all the transformations that have been applied to the object in the order they were applied"
  (let* ((tfms (scale-tfms (small::all-tfms obj) scale))
         (rl (reverse tfms))
         (res (reduce #'small::apply-transformation tfms :initial-value  v :from-end t)))
    res))

(defmethod apply-tfms-in-units-of-a ((obj chem-obj) v)
  "Gets transformations applied to obj, scales the translations so that they are expressed in MEEP/MPBs units of a then applies all transformations to vector v"
  (apply-tfms-scaled obj v *a/nm*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                     ; Meep function calls and class creation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun meep-v3 (&optional (x 0d0) (y 0d0) (z 0d0))
  "Creates a vector used by MEEP/MPB"
  (pyeval "meep.Vector3(" x "," y "," z ")"))

(defun set-python-class-slots (class slots-alist)
  "(car slots-alist)-> quoted slot name
(cdr slots-alist) -> slot value"
  (mapcar #'(lambda (argpair)
              (setf (pyslot-value class (car argpair)) (cdr argpair)))
          slots-alist))

(defun make-mode-solver (&rest rest)
  (when (oddp (length rest)) (error "must supply an even number of rest argument"))
  (let ((modesolver (pyeval "mpb.ModeSolver()"))
        (arg-pairs  (loop :for a :in rest :by #'cddr
                          :for b :in (cdr rest) :by #'cddr
                          :collect (cons a b))))
    (set-python-class-slots modesolver arg-pairs)
    modesolver))

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
  "Creates a dielectric used in MEEP/MPB and specified  in  MEEP/MPB units of a"
  (with-accessors ((eps eps) (center center) (size size)) di
    (let ((asize (nm->a size)) ; We convert to units of a since thats what the meep package requires
          ;; Here we need to make sure that if any translation have been do them in units of a notnm
          (acenter (apply-tfms-in-units-of-a di (nm->a center))))
      (meep:block/class :size (list (x asize) (y asize) (z asize))
                        :center (list (x acenter) (y acenter) (z acenter))
                        :material (meep:medium/class :epsilon eps )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        Photonic Crystal Class       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std photonic-crystal (chem-obj)
  ((cell-size :std (meep:lattice/class :size '(1.2 1.2 1.2)) :doc "size of the unit cell in nm")
   (mode-solver :doc "A meep.MPB.ModeSolver used to compute the modes of the pc. Other args provide setup to this, see get-band-structure")
   (num-bands :std 8 :doc "The number of photonic bands to compute")
   (res :std 32 :doc "The resolution of the meep sim. See:")
   (tm-gaps :doc "The calculated TM band gaps")
   (tm-freqs :doc "The calculated TM frequencies")
   (te-gaps :doc "The calculated TE band gaps")
   (te-freqs :doc "The calculated TE frequencies")
   (kpoints :std (meep:interpolate
                  :n 4
                  :nums (vector (meep-v3)          ;; Gamma
                                (meep-v3 0.5)      ;; X
                                (meep-v3 0.5 0.5)  ;; M
                                (meep-v3)))
            :doc "The kpoints to calculate the bands between in units of a."))
  (:documentation "A class that will perform the simulation of the TM and TE band gaps of a photonic crystal. Any children meep-dielectrics are used for the PC's unit cell."))


(defmethod get-dielectrics ((obj chem-obj))
  "Returns any children that are meep-dielectrics"
  (remove-if-not #'(lambda (child) (typep child 'meep-dielectric))
                 (small::children obj)))

(defmethod get-band-structure ((pc photonic-crystal))
  (with-slots ((ms mode-solver)
               kpoints cell-size res num-bands
               tm-gaps tm-freqs te-gaps te-freqs) pc
    (let* ((geom (apply #'vector (mapcar #'as-meep-dielectric
                                         (get-dielectrics pc)))))
      (setf ms (make-mode-solver
                'resolution res
                'geometry_lattice cell-size
                'geometry geom
                'k_points kpoints
                'num-bands num-bands
                ))
      (pymethod ms 'run_tm
                (mpb:output-at-kpoint (meep-v3 (/ -1 3) (/ 1 3) 0)
                                      #'mpb:fix-efield-phase
                                      #'mpb:output-efield-z))
      (setf tm-freqs (pyslot-value ms 'all_freqs))
      (setf tm-gaps (pyslot-value ms 'gap_list))
      (pymethod ms 'run_te)
      (setf te-freqs (pyslot-value ms 'all_freqs))
      (setf te-gaps (pyslot-value ms 'gap_list))
      (values tm-freqs te-freqs tm-gaps te-gaps))))


(defmethod plot-gaps ((pc photonic-crystal) &optional (filename "result"))
  "Uses matplotlib to plot the bandgaps of the photonic crystal."
  (with-slots (tm-gaps tm-freqs te-gaps te-freqs) pc
    (let* ((x (pycall 'range (pycall 'len tm-freqs)))
           (z (pycall 'zip x tm-freqs te-freqs))
           (subplot-list (plt:subplots))
           (fig (first subplot-list))
           (ax (second subplot-list)))
      ;; NOTE: Be careful about the spacing (because python sucks) of these hacky mixes of lisp and python
      (pymethod fig 'set_figheight 15)
      (pymethod fig 'set_figwidth 25)

      (pyexec "for xz, tmz, tez in " z ": "
              ax".scatter([xz]*len(tmz), tmz, color='blue'); "
              ax".scatter([xz]*len(tez), tez, color='red', facecolors='none')")

      ;;Set colors and axis limits
      (pymethod ax 'plot tm-freqs :color "blue")
      (pymethod ax 'plot te-freqs :color "red")
      (pymethod ax 'set_ylim  (vector 0 1) )
      (pymethod ax 'set_xlim  (vector 0 (1- (pycall 'len x))))
      ;; Plot gaps
      (when tm-gaps
        (pyexec "for gap in " tm-gaps ":
    if gap[0] > 1: " ax ".fill_between(" x ", gap[1], gap[2], color='blue', alpha=0.2)"))
      (when te-gaps
        (pyexec "for gap in " te-gaps ":
    if gap[0] > 1: " ax ".fill_between(" x ", gap[1], gap[2], color='red', alpha=0.2)"))
      ;; Plot labels
      (pymethod ax 'text 5 0.9 "TM bands" :color "blue" :size 60)
      (pymethod ax 'text 10 0.9 "TE bands" :color "red" :size 60)
      (pymethod ax 'set_xticks (pyeval "[i*((len(" tm-freqs ") - 4) / 3)+i for i in range(4)]"))
      (pymethod ax 'set_xticklabels (vector "Γ" "X" "M" "Γ") :size 80)
      (pymethod ax 'set_ylabel "Frequency (c/a)" :size 80)
      (pymethod ax 'grid t)
      ;(pyexec "plt.figure(figsize=(1,1))")
      (plt:savefig (concatenate 'string filename ".pdf")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            Photonic Cube            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std hybrid-cube (meep-dielectric small::dna-cube)
  ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;            DNA-CUBE-ARRAY           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std hybrid-cube-array (small::dna-cube-array photonic-crystal)
  ()
  (:documentation "An array of dielectic cubes"))

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



(setq harr (make-instance 'hybrid-cube-array :cube-type 'hybrid-cube))
(get-band-structure harr)
(plot-gaps harr "DNA27-band-gap")
(show-in-oxview "DNA27-photonic-crystal" (all-array harr))
