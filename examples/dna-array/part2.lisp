;;;; # Introduction
;; In this part of the tutorial we will use the `dna-triangle`s that we made in Part 1 to make the DNA tile of [Tikhomirov et al](https://doi.org/10.1038/nnano.2016.256), shown below. To do so we will use 4 of the `dna-triangle` created in part 1 to implement a the four triangles that make up the tile. We create a `dna-tile` class to create the abstraction  that representing a DNA tile.

;; ![DNA Tile](greg-tile.png)

;; During this we show case some of the geometric manipulation functionality that `small` provides and takes a deeper look at the parent-child hierarchy. Finally we will demonstrate how to create staples to join the triangle and tiles together and how to set the sequence of our `dna` objects.

;; Lets get started! First we need to import the definitions of the `dna-triangle` that we created in Part 1. To do we we use the `load` function to load in the whole file. Note we load the .lisp file with the code from part, not the .ipynb file we were working on, although these contain the same code.

(in-package :small)
(load "./part1.lisp")

;;;; # Creating The `dna-tile` Classs
;; First we create a class to hold the DNA Tile abstraction. This is similar to what we did in part 1 where we created the `dna-triangle` to represent the DNA triangle.

(defclass/std dna-tile (dna-origami)
  ((t1 :doc "triangle 1" :std (make-instance 'dna-triangle))
   (t2 :doc "triangle 2" :std (make-instance 'dna-triangle))
   (t3 :doc "triangle 3" :std (make-instance 'dna-triangle))
   (t4 :doc "triangle 4" :std (make-instance 'dna-triangle))
   (stap-bridges :doc "Staple Bridges"))
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

;;  Notice that now we have added the custom slots `t1-t4` to our `dna-tile` class, whereas when defining the  `dna-triangle` we only had the slots we inherited from the `dna-origami` superclass. These slots are each initialized to hold an instance of the `dna-triangle` class, which we use to construct the tile the tile.

;; Next we rotate the `dna-triangle`s to position them to form the `dna-tile`.

(defmethod initialize-instance :after ((obj dna-tile) &key)
  (with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) obj
    (let* ((rot90 (rotation-matrix (v3 0 1 0) (/ pi -2)))
	   (rot180 (rotation-matrix (v3 0 1 0) pi))
	   (rot270 (rotation-matrix (v3 0 1 0) (/ (* 3 pi) -2))))
      (rotate-obj t2 rot90)
      (rotate-obj t3 rot180)
      (rotate-obj t4 rot270))))

;; We used the [`with-accessors` function](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_acce.htm) to retrieve the objects in the `t1`-`t4` slots of the `dna-tile`. `with-accessors` first argument is a list where each element of the list is a two-item list containing a variable name and the name of an accessor function for a class, the second is an instance of that class. The result is that the variable are bound to the slots of the object passed as the second argument (this is similar to the `let` function in that it binds variables). For more information on classes in Common Lisp [see here](http://www.gigamonkeys.com/book/object-reorientation-classes.html).

;; We then create rotation matrices to represent the rotation we want performed on the `dna-triangle`s. `small` provides the `rotation-matrix` function (defined in [linear-algebra.lisp file](https://github.com/DurhamSmith/small/blob/master/linear-algebra.lisp)) to easily create rotation matrices. It takes a vector, which is the axis to rotate around and an angle in radians and returns a 3x3 matrix representing a rotation of the angle around the

;; Here we have used the `rotate-obj` function to rotate triangles 2-4. Applying geometric manipulations (translation, rotation) on a `chem-obj` (the fundamental superclass that `small` uses to represent chemical entities) that is `parent` to other objects effects all the `children`, however geometrically manipulating a child has no effect on the `parent` objects coordinates, only on that of the children. The function `all-tfms` retrieves the list transformations that will be applied to the object. We can see this parent-child behavior;

(with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) (make-instance 'dna-tile)
  (format t "~& T2: ~A ~%" (all-tfms t2))
  (format t "~& 5nt:T2 ~A ~%" (all-tfms (5nt t2)))
  (format t "~& Now we rotate the child  ~%")
  (rotate-obj (5nt t2) (rotation-matrix (v3 0 1 0) pi))
  (format t "~& T2: ~A ~%" (all-tfms t2))
  (format t "~& 5nt:T2 ~A ~%" (all-tfms (5nt t2))))

;; We see that even after we rotate the `5nt` (the `dna-nt` at the 5' end of strand) the transformations applied to its `parent`, the `dna-triangle`, remain the same.

;; We can write the `dna-tiles` nucleotides out and see the tile that we have created in applicable viewer such as oxview.
(with-accessors ((t1 t1) (t2 t2) (t3 t3) (t4 t4)) (make-instance 'dna-tile)
  (wmdna "tile-v0" t1 t2 t3 t4))

;; ![DNA Tile V0](tile-v0.png)

;;;; # Adding Scaffold Bridges
;; We see that the triangles (and the `dna-helix-strand`s and `dna-nt`s they contain) have been rotated. Notice how we have retrieved each individual triangle from the tile and written it out. This is because currently the triangle are disconnected entities. We fix this by adding each triangle to the `dna-tile`s scaffold using the `add-to-scaffold` function (introduced in part 1) provided by the `dna-origami` class (that we used as a superclass when creating the tile). We redefine the `initialize-instance` function
;; specialized on `dna-tile` so that the `dna-triangle`s are added to the scaffold when we create an instance of `dna-tile` instead of having to do it manually every time.

(defmethod initialize-instance :after ((obj dna-tile) &key)
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


;; This has set the `5nt` and `3nt` of the `dna-tile`, as well as connected the ith triangle to the i+1th when they were added to the scaffold. Again we can write this to a file and see the results, this time without needing to retrieve all the `dna-triangle`s individually.
(wmdna "tile-v1"  (make-instance 'dna-tile))
;; ![DNA Tile V1](tile-v1.png)

;; All looks good! Or does it? When we closely examine the connections between `dna-triangles` we notice have forgotten to add the scaffold bridges that join `dna-triangle` i to `dna-triangle` i+1. Not to worry this is easily fixed. Again we will use the `bridging-single-strand` function (introduced in part 1), this time explicitly passing the number of nucleotides to be 10 (as in the paper see SI p9). Lets create a function to do this.

;; First we define a utility function to retun `dna-triangle` `k` when the `dna-tile` and triangle index `k` (1-4)

(defun get-triangle (tile k)
  "Utility function to retrieve triangle k from the tile"
  (cond ((= k 1) (t1 tile))
	((= k 2) (t2 tile))
	((= k 3) (t3 tile))
	((= k 4) (t4 tile))
	(t (error "k=~A is not a valid triangle index"))))

;; Now we can define the function to create the scaffold bridge. Its arguments are a `dna-tile` and triangle index `k` and returns a `dna-single-strand` the spans the distance from the 3' end of the kth triangle to the 5' end of the k+1th triangle.

(defun tile-stap-bridge (tile k)
  "Returns a DNA-SINGLE-STRAND that connects triangle k to triangle k+1 in the tile"
  (let* ((tk (get-triangle tile k))
	 (tk+1 (get-triangle tile (1+ k)))
	 (axis-k (axis (3nt tk)))   ;; Retrive the helix axis coords of last nucleotide in triangle k
	 (axis-k+1 (axis (5nt tk+1)))) ;; Retrive the helix axis coords of first nucleotide in triangle k+1
    (bridging-single-strand axis-k axis-k+1 (v3 0 1 0) :len 10)))

;; In defining `tile-stap-bridge` we used the function `axis` which takes a `dna-nt` and returns the coordinates of its axis. `small` provides many such function for the retrieval of geometric points, one can look at the [dna.lisp](https://github.com/DurhamSmith/small/blob/master/dna.lisp) file to find more such functions. Lets check that this works as expected

;; (tile-stap-bridge (make-instance 'dna-tile) 1)

;; We see that `tile-stap-bridge` makes use of Common Lisps ability to return multiple value. It returns a  `DNA-SINGLE-STRAND` and a list of the `DNA-NT`s that are contained in it. We see that there are 10 DNA as expected.

;; Now lets modify our initialization of the `dna-tile` to add the scaffold bridges to our scaffold.


(defmethod initialize-instance :after ((obj dna-tile) &key)
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

;; Let write out the `dna-tile` and see thta the scaffold bridges are included.
(wmdna "tile-v2"  (make-instance 'dna-tile))
;; ![DNA Tile V2](tile-v2.png)
;; Great! The staple bridges are included and everything is as expected

;;;; # Adding Staples
;; Next we want to add some staple bridges to hold the `dna-tile` together. To do so we will define a function that takes a `dna-tile` the tile number `k` (values: [1,4]) and a helix number `i` (values [12,22]) and keyword arguments `:len1` and `:len2` and returns a `dna-staple-strand` that has its helical partners on the `k`th triangles `i`th helix and the `k+1`th triangle `(23-i)`th helix. The keyword arguments `:len1` and `:len2` are used to specify the length of the staple of on the `k`th and `k+1`th triangle respectively.

;; First we define a function that takes triangle index `k` and returns the index of the triangle that helices 12-22 of the triangle should be connected to. We cannot simply add 1 to the index since triangle 4 is connected to triangle 1.

(defun next-triangle-index (k)
  (cond ((= k 1) 2)
	 ((= k 2) 3)
	 ((= k 3) 4)
	 ((= k 4) 1)
	 (t (error "Invalid index ~A" (= k 1)))))

;; Now we can define the function that creates the `dna-staple-strand`.

(defun stap-bridge (tile k i &key (len1 8) (len2 8))
  (if (or (> i 11) (> 23 i))
      (let* ((hel1 (find-obj-with-props (scaffold (get-triangle tile k))
					`((:i . ,i))))
	     (hel2 (find-obj-with-props (scaffold (get-triangle tile (next-triangle-index k)))
					`((:i . ,(- 23 i)))))
	     (stap (if (oddp i)
		       (create-staple `((:obj ,hel1 :start 0 :end ,len1 :from-3end nil)
					(:single-strand t)
					(:obj ,hel2 :start 0 :end ,len2 :from-3end t)))
		       (create-staple `((:obj ,hel2 :start 0 :end ,len2 :from-3end nil)
					(:single-strand t)
					(:obj ,hel1 :start 0 :end ,len1 :from-3end t))))))
	stap)
      (error "Not a valid row selection")))

Before incorporating this into `initialize-instance` specialized on `dna-tile` lets print out the results of the staple bridges created on each triangle.

(let* ((tile (make-instance 'dna-tile))
       (sb1 (stap-bridge tile 1 12))
       (sb2 (stap-bridge tile 2 13))
       (sb3 (stap-bridge tile 3 14))
       (sb4 (stap-bridge tile 4 15)))
  (wmdna "tile-v3" tile sb1 sb2 sb3 sb4))

;; ![DNA Tile V3](tile-v3.png)
;; Everything works as expected. Let unpack what is going on in the `stap-bridges` function. There are two new and important functions that we use here, `find-obj-with-props` and `create-staple`. Before we dive into them it is important to understand the `` ` `` function and its use. If you're still unfamiliar re-read the Introduction to Common Lisp section in part 1.

;; Armed with understanding of `` ` ``s behavior are in a position to understand what is going on in the `find-obj-with-props` and `create-staple` functions. First `find-obj-with-props` is a function implemented by the the `chem-obj` class. The `chem-obj` class forms the lowest level of abstraction for building chemical entities in `small`. `find-obj-with-props` takes a list of `chem-obj`s and a list of key-value pairs, as `cons cells` and returns the first `chem-obj` in the that contains all the given key-val pairs. Without getting into to much detail on the `cons cell` data structure for our purposes we can create a `cons cell` using the `(key . value)` or `(cons key val)`. Lets see how this works;
(find-obj-with-props (scaffold (t1 (make-instance 'dna-tile))) '((:i . 1)))

;; In the call to `find-obj-with-props` we retrieve all the `scaffold` helices for a `dna-triangle` within the `dna-tile` `(scaffold (get-triangle tile k))` and then query for the `i`-th helice. We do the same for the `k+1`th triangle and its `(- 23 i)`th helice. We store and retrieve these helices so we can add staples to hold them together. We do us using the `create-staple` function.

;; `create-staple` take a nested list that specifies what the staple should look like. The entries in the nested list can be of two forms. The fist is `(:obj ,hel1 :start 0 :end ,len1 :from-3end nil)` where :obj is a `DNA-HELIX-STRAND` to which the staple strand should be made, `:start` specifies the 0 indexed inclusive position from the 5' prime end of the `DNA-HELIX-STRAND`, `:end` specifies the 0 indexed excluded end position of what nucleotide sequence strand. The `:from-3end` keyword changes the behaviour of the `:start` and `:end` keywords to traverse the strand in the 3'->5' direction. 

;; `create-staple` takes a nested list where each inner list can take one one of two forms. The first of these forms is `(:obj DNA-HELIX-STRAND  :start INT :end INT  :from-3end BOOL)` which will create a `DNA-STAPLE-STRAND` to the  `DNA-HELIX-STRAND` that follows the `:obj` keyword. The `:start` keyword specify the zero indexed inclusive starting bound and excluded ending bounds, as traversed starting at the 5' end of the `DNA-HELIX-STRAND`. The `from-3end` keyword argument when `t` modifies this behaviour traversing the strand from the 3' end instead.

;; The second type of list that `create-staple` can accept has the form `(:single-strand t [opt] :num-nts INT/nil)` which will create a `DNA-SINGLE-STRAND` that connects the `DNA-HELIX-STRAND` of the staple that is that specified by the immediately preceding entry of the list passed to `create-staple` to the `DNA-HELIX-STRAND` specified by the list entry that immediately follows the `(:single-strand t)` entry. If the `:num-nts` keyword is passed the `DNA-SINGLE-STRAND` will contain than many `DNA-NT`s. If this argument is not passed or is `nil` the number of `DNA-NT`s will be calculated based on the Euclidean distance between the 3' and 5' ends of the `DNA-HELIX-STRAND`s that the `DNA-SINGLE-STRAND` connects.

;; We can see this behavior better by zooming in on the created staple
;; ![DNA Tile V3 Staples](tile-v3-zoomed.png)



; LocalWords:  dna accessors accessor
