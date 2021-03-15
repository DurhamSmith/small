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

(defun next-triangle-index (k)
  (cond ((= k 1) 2)
	 ((= k 2) 3)
	 ((= k 3) 4)
	 ((= k 4) 1)
	 ((t (error "Invalid index ~A" (= k 1))))))



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
	(format t "~& aoeusntaoe ~%")

	stap)
      (error "Not a valid row selection")))

;;;; Before incoporating this into the class initialization lets quickly print out the results on the two cases of scaffold bridge 

(let* ((tile (make-instance 'dna-tile2))
       (sb1 (stap-bridge tile 1 12))
       (sb2 (stap-bridge tile 2 13)))
  (wmdna "tile-v3" tile sb1 sb2))

;;;; Everthing works as expected. Let unpack what is going on in the stap-bridges function. There are two new and important functions that we use here, (find-obj-with-props) and (create-staple). Before we dive into them it is important to understand a little bit about lisp, since we make some use of language features that might be unfamiliar to those coming into programming from an ALGOL derived langage.

;;;; In lisp code and data is represented as a list. Representing code and data in the same form is called homoiconcity and is a large source of lisps power and expressiveness. For example, code to call a function is composed of a list, with the first list entry being the fuction name and the next entries being its arguments. E.g.
(+ 1 2)
;;;; Data is also composed of lists, say for example a list of vaules (1 2). If we wanted to create a list with data we cannot simply type (1 2) as when lisp interprets this first value in the list as the function name. 
(1 2)
;;;; To get around this we can use the quote function. It takes one argument and returns exactly that argument, without evaluating anything in it or trying to interpret first list entry as a function name.
(quote (+ 1 2))
(quote (:i (+ 1 2) (+ 2 3)))
;;;; Since lists are so prolific in lisp the syntax has a lot of synatctic sugar to deal with their manipulation. For example instead or writing quote we can just write ' before the argument we normally pass to quote.
'(:i (+ 1 2) (+ 2 3))
;;;; Notice how the (1+ 1) hree is not evaluated. If we did want it to be evaluated we could use the function list which takes n arguments, evaluates them and returns the values resulting from evaluating them in a list of length n.
(list :i (+ 1 2) (+ 2 3))
;;;; Another sytactic sugar that we use is backquote. We need to understand its use if we want to understand create-staple and find-obj-with-props. Just like the syntactic sugar for quote, ', infact using it in place of quote yeilds the same result
`(:i (+ 1 2) (+ 2 3))
;;;; Where its behavior changes is when a TODO (argument?) is preceeded by a ,. When this happens the expression is evaluated and placed in the resulting list, e.g.
`(:i (+ 1 2) ,(+ 2 3))


;;;; Now we  are in a positino to understand what is going on in the find-obj-with-props and create-staple. First find-obj-with-props is a function we get access to from the chem-obj class that forms the lowest level of abstraction for building chemical entities in small. find-obj-with-props takes a list of chem-objs and a list of key-value pairs, as cons cells and returns the first object that contains them. Without getting into to much detail on the cons cell data structure for our purposese we can create a cons cell using the (key . value) synatax or (cons key val). So in the call to find-obj-with-props we retrieve all the scaffold helices for a triangle within a tile (scaffold (get-triangle tile (next-triangle-index k))) and then query for the i-th helice and the (- 23 i)th of the triangle that its right edge (helices 12-22) (remebering that , in a backtick environment causes evaluation of the expression). We store and retrieve these helices so we can add staples to hold them together.

;;;; create-staple take a nexted list that specifies what the staple should look like. The entries in the nested list can be of two forms. The fist is (:obj ,hel1 :start 0 :end ,len1 :from-3end nil) where :obj is a DNA-HELIX-STRAND to which the staple strand should be made, :start specifies the 0 indexed inclusive position from the 5' prime end of the DNA-HELIX-STRAND, :end specifies the 0 indexed excluded end position of what nucleotide sequence strand. The :from-3end entry changes the behaviour of the :start and :end keywords to traverse the strand in the 3'->5' direction. The second entry the list can t

;;  \lstinline{create-staple} takes a nested list where each inner list can take one one of two forms. The first of these forms is
;; \lstinline{(:obj DNA  :start INT :end INT  :from-3end BOOL)} which will create a staple strand the \lstinline{dna-strand} that follows the \lstinline{:obj} keyword. The \lstinline{:start} keyword specify the zero indexed inclusive starting bound and excluded ending bounds, as traversed starting at the \(5^{\prime}\) end of the \lstinline{dna-strand}. The \lstinline{from-3end} keyword argument when \lstinline{t} modifies this behaviour traversing the strand from the \(3^{\prime}\) end instead.

;; The second type of list that \lstinline{(create-staple)} can accept has the form \lstinline{(:single-strand t [opt] :num-nts INT/nil)} which will create a \lstinline{dna-single-strand} that connects the \lstinline{dna-helix-strand} of the staple that is that is \textcolor{red}{created by the list entry just before it in the nested list} passed to \lstinline{create-staple} to the \lstinline{dna-helix-strand} that is created in the following nested list entry. If the \lstinline{:num-nts} keyword is passed the \lstinline{dna-single-strand} will contain than many \lstinline{dna-nt}s. If this argument is not passed or is \lstinline{nil} the number of \lstinline{dna-nt}s will be calculated based on the Euclidian distance between the \(3^{\prime}\) and \(5^{\prime}\) end of the \lstinline{dna-helix-strand}s that this \lstinline{dna-single-strand} connects.


