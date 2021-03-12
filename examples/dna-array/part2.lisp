(in-package :small)
;;(load "./part2.lisp")
;;;; In this section of the tutorial we will take the dna-triangle and use it create a DNA tile of Tikhomirov et al (TODO REF). First we will use 4 of the dna-triangle2 created in part 1 to implement a dna-tile class that provides an abstraction representing a DNA tile. During this we show somecase some of the geometric manipulation functionality that small provides and takes a deeper look at the parent-child heirarchy. Next we will demonstrate how to create staples to join the triangle and tiles together.

;;;; First we create a class to hold the dna-tile abstraction, as we did in part 1 with for the class we created to represent the dna-triangle.

(defclass/std dna-tile2 (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle2))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle2))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle2))
   (t4 :doc "triangle 4" :std (make-instance 'dna-triangle2))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

;;;; Notice that now we have added the custom slots t1-t4 to our dna-tile class. These slots are each initalized to hold the dna-triangle that we created in part 1.
;;;; Next we rotate and translate the dna-triangles to position them to form the dna-tile. 

(makunbound 't2)
(defmethod initialize-instance :after ((obj dna-tile2) &key)
  (with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) obj
    (let* ((rot90 (rotation-matrix (v3 0 1 0) (/ pi -2)))
	   (rot180 (rotation-matrix (v3 0 1 0) pi))
	   (rot270 (rotation-matrix (v3 0 1 0) (/ (* 3 pi) -2))))
      (rotate-obj t2 rot90)
      (rotate-obj t3 rot180)
      (rotate-obj t4 rot270))))

(with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) (make-instance 'dna-tile2)
  (wmdna "tile2" t1 t2 t3 t4))




       
