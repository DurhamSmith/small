(progn
  (ql:quickload :small)
  (in-package :small)
  (load "/home/dd/quicklisp/local-projects/small/examples/dna-array/utils.lisp")
  (load "/home/dd/quicklisp/local-projects/small/examples/dna-array/dna-triangle.lisp")
  (load "/home/dd/quicklisp/local-projects/small/examples/dna-array/dna-corner.lisp")
  (load "/home/dd/quicklisp/local-projects/small/examples/dna-array/dna-cube.lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;       DNA-CUBE-ARRAY CLASS DEF      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-cube-array (dna-origami)
  ((cube-type :doc "class identifier of the type of cube that should be instantiate"
              :std 'dna-cube)
   (numx :doc "Number of cubes in the x direction" :std 2)
   (numy :doc "Number of cubes in the y direction" :std 2)
   (numz :doc "Number of cubes in the z direction" :std 2)
   (tx :doc "Number of nanometers to translate in the x direction"
       :std (+ (* (cos (/ pi 4)) *w*) 5))
   (ty :doc "Number of cubes in the z direction"
       :std (+ (* (cos (/ pi 4)) *w*) 5))
   (tz :doc "Number of cubes in the z direction"
       :std (+ (* (cos (/ pi 4)) *w*) 5)))
  (:documentation "An array of DNA cubes"))

(defmethod initialize-instance :after ((array dna-cube-array) &key)
  (with-accessors ((nx numx) (ny numy) (nz numz)
                   (tx tx) (ty ty) (tz tz) (cube-type cube-type)) array
  (loop for ix from 0 below nx
        collect (loop for iy from 0 below ny
                      collect (loop for iz from 0 below nz
                                    collect (let ((cube (make-instance cube-type))
                                                  (tvec (v3
                                                         (* ix tx)
                                                         (* iy ty)
                                                         (* iz tz))))
                                              (translate-obj cube tvec)
                                              (add-child array cube)
                                              (add-prop cube :array-index (list ix iy iz)))))))
  array)

(memory::dump-memory (make-instance 'dna-cube-array))
(setq tst (make-instance 'dna-cube-array))
(setq t16 (make-instance 'dna-cube-array :numx 4 :numy 4 :numz 4))

(wmdna "/home/dd/quicklisp/local-projects/small/examples/dna-array/tmp/arr"
       (mapcar #'all-cube (children t16)))




(time (make-instance 'dna-cube))
(sbcl:gc :full t)
(sb-ext:gc)
(let (items)
  (sb-ext:gc) ; name varies by implementation
  (room)
  (dotimes (x 1)
    (push (make-instance 'dna-cube) items))
  (sb-ext:gc)
  (room)
)
