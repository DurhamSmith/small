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
;;;; Next we rotate the dna-triangles to position them to form the dna-tile. 

(makunbound 't2)
(defmethod initialize-instance :after ((obj dna-tile2) &key)
  (with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) obj
    (let* ((rot90 (rotation-matrix (v3 0 1 0) (/ pi -2)))
	   (rot180 (rotation-matrix (v3 0 1 0) pi))
	   (rot270 (rotation-matrix (v3 0 1 0) (/ (* 3 pi) -2))))
      (rotate-obj t2 rot90)
      (rotate-obj t3 rot180)
      (rotate-obj t4 rot270))))

;;;; Here we have used the rotate-obj function to rotate triangles 2-4. Rotating an object that is parent to other objects effects all the children, however appling a transformation on child has no effect on the parent objects coordinates, only on that of the children. The function all-tfms retrieves the list transformations that will be applied to the object. We can see this parent-child behavior;

(with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) (make-instance 'dna-tile2)
  (format t "~& T2: ~A ~%" (all-tfms t2))
  (format t "~& 5nt:T2 ~A ~%" (all-tfms (5nt t2)))
  (rotate-obj (5nt t2) (rotation-matrix (v3 0 1 0) pi))
  (format t "~& T2: ~A ~%" (all-tfms t2))
  (format t "~& 5nt:T2 ~A ~%" (all-tfms (5nt t2))))



;;;; We can write the tiles nucleotides out and see the tile that we have created in applicable viewer such as oxview.
(with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) (make-instance 'dna-tile2)
  (wmdna "tile-v0" t1 t2 t3 t4))


;;;; We see that the triangles (and the DNA-HELIX-STRAND and DNA-NTs) have been rotated. Notice how we have retrieved each individual triangle from the tile and written it out. This is because currently the triangle are disconnected entities. We fix this by adding each triangle to the tiles scaffold using the add-to-scaffold function provided by the dna-origami class (that we used as a superclass when creating the tile). Again we use the add-to-scaffold function introduced in part 1.
       
(defmethod initialize-instance :after ((obj dna-tile2) &key)
  (with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) obj
    (let* ((rot90 (rotation-matrix (v3 0 1 0) (/ pi -2)))
	   (rot180 (rotation-matrix (v3 0 1 0) pi))
	   (rot270 (rotation-matrix (v3 0 1 0) (/ (* 3 pi) -2))))
      (rotate-obj t2 rot90)
      (rotate-obj t3 rot180)
      (rotate-obj t4 rot270))
    (add-to-scaffold obj t1)
    (add-to-scaffold obj t2)
    (add-to-scaffold obj t3)
    (add-to-scaffold obj t4)))


;;;; This has set the 5nt and 3nt of the tile, as well as connected the ith triangle to the i+1th as they were added to the scaffold. Again we can write this to a file and see the results.
(wmdna "tile-v1"  (make-instance 'dna-tile2))
p
;;;; All looks good! Or does it? We have forgotten to add the scaffold bridges that join triangle i to triangle i+1. Not to worry this is easily fixed. Again we will use the bridging-single-strand function, this time explicitly passing the number of nucleotide to be 10 as in the paper (see SI p9). We create a function to do this, 

(defun get-triangle (tile k)
  "Utility function to retrieve triangle k from the tile"
  (cond ((= k 1) (t1 tile))
	((= k 2) (t2 tile))
	((= k 3) (t3 tile))
	((= k 4) (t4 tile))
	(t (error "k=~A is not a valid triangle index"))))

(defun tile-stap-bridge (tile k)
  "Returns a DNA-SINGLE-STRAND that connects triangle k to triangle k+1 in the tile"
  (let* ((tk (get-triangle tile k))
	 (tk+1 (get-triangle tile (1+ k)))
	 (axis-k (axis (3nt tk)))   ;; Retrive the helix axis coords of last nucleotide in triangle k
	 (axis-k+1 (axis (5nt tk+1)))) ;; Retrive the helix axis coords of first nucleotide in triangle k+1
    (bridging-single-strand axis-k axis-k+1 (v3 0 1 0) :len 10)))

;;;; In this we used the function axis which takes a DNA-NT and returns the coordinates of its axis. small provides many such function for the retireval of geometric points, one can look at the dna.lisp file to find more such functions. Lets check that this works as expected

(tile-stap-bridge (make-instance 'dna-tile2) 1)

;;;; We see we get DNA-SINGLE-STRAND and that there are 10 nucleotides as expected. Now lets modify our initialization of the DNA-TILE

	 
(defmethod initialize-instance :after ((obj dna-tile2) &key)
  (with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) obj
    (let* ((rot90 (rotation-matrix (v3 0 1 0) (/ pi -2)))
	   (rot180 (rotation-matrix (v3 0 1 0) pi))
	   (rot270 (rotation-matrix (v3 0 1 0) (/ (* 3 pi) -2))))
      (rotate-obj t2 rot90)
      (rotate-obj t3 rot180)
      (rotate-obj t4 rot270))
    (add-to-scaffold obj t1)
    (add-to-scaffold obj (tile-stap-bridge obj 1))
    (add-to-scaffold obj t2)
    (add-to-scaffold obj (tile-stap-bridge obj 2))
    (add-to-scaffold obj t3)
    (add-to-scaffold obj (tile-stap-bridge obj 3))
    (add-to-scaffold obj t4)))

;;;; Now we can write the new tile out and check that everything works as expected
(wmdna "tile-v2"  (make-instance 'dna-tile2))

;;;; We see that the scaffold bridges are included. TODO ADD FIG

;;;; Next we want to add some staple bridges to hold the tile together. To do so we will define a function that take a dna-tile the tile number k [1,4] and a helix number i [12,22] and keyword arguments :l1 and :l2 returns a staple that between the kth triangle ith helix and the k+1th triangle 23-ith helix.

(find-obj-with-props (scaffold (t1 (make-instance 'dna-tile2))) '((:i . 1)))

(defun stap-bridge (tile k i &key (len1 8) (len2 8))
  (if (or (> i 11) (> 23 i))
      (let* ((hel1 (find-obj-with-props (scaffold (get-triangle tile k))
					`((:i . ,i))))
	     (hel2 (find-obj-with-props (scaffold (get-triangle tile (1+ k)))
					`((:i . ,(- 23 i)))))
	     (stap (if (oddp i)
		       (create-staple `((:obj ,hel1 :start 0 :end ,len1 :from-3end nil)
					(:single-strand t)					
					(:obj ,hel2 :start 0 :end ,len2 :from-3end t)))
		       (create-staple `((:obj ,hel2 :start 0 :end ,len2 :from-3end nil)
					(:single-strand t)					
					(:obj ,hel1 :start 0 :end ,len1 :from-3end t))))))
	(format t "~& aoeusntaoe ~%")

	stap)
      (error "Not a valid row selection")))

(let* ((tile (make-instance 'dna-tile2))
       (sb1 (stap-bridge tile 1 12))
       (sb2 (stap-bridge tile 2 13)))
  (wmdna "tile-v3" tile sb1 sb2))

;;;; 



    
    
  


  

