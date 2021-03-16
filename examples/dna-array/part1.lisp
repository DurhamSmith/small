;;;; # Introduction
;; This tutorial aims to show how a 8x8 array of DNA tiles based on the work of Tikhomirov et al, shown below, can be created in `small`.
;; ![title](tile-array.png)
;; The tutorial is divided into 3 parts, each aimed at introducing some of `small`s capabilites and the key ideas behind them while laying down the code needed to create an array of DNA tiles.
;; In the first part we cover the basic syntax of `common lisp` so that even those unfamilar with the language may easily follow along. Along side this we introduce the DNA model that `small` uses to represent DNA, showcase some of the API functionality for creating representations of objects made of DNA and define our own such object to represent the scaffold of one of the four triangles that a single tile in the array is made of. 

;; The next section shows how `small` allows one to build nanostructures of ever increasing size. We learn how to geometrically manipulate the triangle we created in part 1 and how to connect four such triangles to create a full tile used in the 8x8 array. We show how we can associate key-value pairs to parts of DNA structures and how we can retrieve structures based on these key-value pairs. Additionally how to create staple strands to hold the created tile together is also shown.

;; Part 3 shows how these tiles are in turn used to create a DNA array and how staple strands can be extended to create arbitrary patterns on these arrays.

;;;; # Introduction to Common Lisp
;; Those unfamiliar with Lisp might be confused by the syntax and the abundance of parenthesis. This is no reason for concern, in-fact it is what give the Lisp family of languages their incredible expressive power. Lisp code is comprised of s-expressions, which can either be lists or atoms. Lists are delimited by parentheses and can contain any number of whitespace-separated elements. Atoms are everything else, for example a number, a character or a user defined object. Code and data are represented by lists, hence the large amount of parenthesis.

;; Code to call a function is composed of a list, with the first list entry being the function name and the next entries being its arguments. E.g.

(+ 1 2)

;; When evaluating multiple lists the inner most lists are evaluated before the enclosing list. E.g;

(- (+ 2 3) (+ 1 2))

;; Lists can also contain data, say for example a list of values (1 2) or all the nucleotides in a strand of DNA. If we wanted to create a list with data we cannot simply type (1 2) as Lisp will try to interpret first value in the list as the function name, resulting in an illegal function call `ERROR`.
(1 2)

;; To get around this we can use the `quote` function. It takes one argument and returns exactly that argument, without evaluating anything in it or trying to interpret first list entry as a function name.
(quote (+ 1 2))
(quote (:i (+ 1 2) (+ 2 3)))

;; Since lists are so prolific in Lisp the syntax has a lot of syntactic sugar to deal with their manipulation. For example instead or writing quote we can just write ' before the argument we normally pass to quote.
'(:i (+ 1 2) (+ 2 3))

;; Notice how the lists containing the `+` are not evaluated. If we did want it to be evaluated we could use the function `list` which takes n arguments, evaluates them and returns the values results from evaluating them in a list of length n.
(list :i (+ 1 2) (+ 2 3) 10 "DNA nanotechnology is fun!")

;; Another syntactic sugar that we use is backquote, `` ` ``. We need to understand its use if we want to understand create-staple and find-obj-with-props. Just like the syntactic sugar for quote, `'`, in fact using it in place of quote yields the same result.
`(:i (+ 1 2) (+ 2 3))

;; Where its behavior of `` ` `` is different to that of `'` is when an s-expression in the list is preceded by a `,`. When this happens the expression is evaluated and placed in the resulting list, e.g.
`(:i (+ 1 2) ,(+ 2 3))


;;;; # Introduction to `small`

;; First we need to tell Common Lisp that we to use of the `small` package to gain access to its functionality it provides. First we load the package using [quicklisp](https://www.quicklisp.org/beta/).
;; (ql:quickload :small)

;; Next we specify that we want to be in the `small` package so that we are able to use the functions and global variable that are defined in it.

(in-package :small)

;;; ## Creating constants
;; In this section we  introduce the constants defined in [Tikhomirov et al](https://doi.org/10.1038/nnano.2016.256) that implement the geometric model of DNA Square (below) that they use.
;; ![DNA Tile](greg-tile.png)

;; See [pages 7-11 of the supplementary information](https://static-content.springer.com/esm/art%3A10.1038%2Fnnano.2016.256/MediaObjects/41565_2017_BFnnano2016256_MOESM28_ESM.pdf) for information on the geometric model. For convenience we repeat the definition of the constants;

;; ![title](consts.png)


;; To do so we use the [`defparameter` function](http://clhs.lisp.se/Body/m_defpar.htm), which binds a value to a variable. The list items that follow are the arguments to the function. In this case the fist argument is the variable name and the second argument is value to bind to it. Lisp also the ability code to add documentation to itself and the third argument is a docstring that adds such documentation describing the introduced variable. The surrounding *'s in the name of the variable is a convention in Common Lisp for denoting global variables.

;;  Lets define the constants;


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

;; We see that the variables have been correctly introduced.

;;;;## Calculating Number of Base Pairs Per Helix
;; Next we define the `ai` function (from the extract of the supplementary info above) that calculates the number of base pairs in each row of the double helix. To define a function we use the [defun](http://clhs.lisp.se/Body/m_defun.htm) function. which as its first argument takes the function name, then a list of variable that the function accepts (in this case only the row number `i`) followed by an optional documentation string and the body of the function.

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

;; Notice how we use the global variables `*helix-diameter*` `*helix-spacing*` and `*helix-nt-spacing*`. These are variable provided by `small` (see the [dna.lisp](https://github.com/DurhamSmith/small/blob/master/dna.lisp) file) that defines common parameters of DNA.

;; Also since we added a docstring to the function we are able to retrieve this (and other) information about the function using the `describe` function.

(describe #'ai)

;; If we only wanted the documentation can use the `documentation` function.

(documentation #'ai t)

;; Notice since we wanted information on a function we use the `#'` syntax, this means we want the information on the function contained in the symbol named `ai`. Had we not used this we would have run into an error since we would try to retrieve information on the variable (not the function).
(describe ai)

;;`small` is fully documented and the documentation can be viewed at anytime using `describe` and `documentation` or viewed using a browser at the official documentation page. ;TODO: 

## Functions for Helix Axis Coordinates
Next we will define functions that caluclate the coordinates of the helix axis for nucleotide number `j` in helix `i` of the DNA triangle composing the square.

These are defined on page 8 of the supplementary information but are also given in the figure below. Notice that in the paper coordinate system is not defined in conventional cartesian coordinate system. As such we translate the coords in the pape to a normal cartesian coordinate system by reflecting the y-vaules around the origin. Although this does not matter for the coordinates for the helices axis since these are defined to be in the y=0 plane.

![Scaffold Helix Coords Calculation](greg-tile.png)

We define functions to calculate the `x`,`y` and `z` coordinates of the helix axis and a function that then uses these to return a vector with these coordinates. 

(defun tri-ax-x (i)
  "Calculate the x coordinate (left-right) of the helix axis in the two-dimensional plane of the  bases pair in the i-th row in the 'top' triangle [0-3] going clockwise with 0 at top
Returns: float (x with)
Note: The geometric model in https://www.nature.com/articles/nnano.2016.256 defines the coordinate system"
  (float (+ (- (/ *w* 2))
	    *i1*
	    *g*
	    (* (+ *helix-diameter* *helix-spacing*)
	       (- i 1)))))


(defun tri-ax-y ()
  "Calculate the y coordinate (in/out) in the 'top' triangle [0-3] going clockwise with 0 at top
Returns: float (y)
Note: The geometric model inhttps://www.nature.com/articles/nnano.2016.256 defines the coordinate system and y is constant at 0 for the helix axis"
  0d0)

(defun tri-ax-z (j)
  "Calculate the z coordinate (up/down) of the helix axis in the two-dimensional plane of the  
j-th base pair in a row of the 'top' triangle triangle [0-3] going clockwise with 0 at top
Returns: float (y)
Note: The geometric model inhttps://www.nature.com/articles/nnano.2016.256 defines the coordinate system"
    (float (+ (- (/ *w* 2))
	      (* *helix-nt-spacing* j))))



(defun tri-ax-coords (i j)
  "The coordinate location of the helix axis in the two-dimensional plane of the j th base pair in the i th row in the first triangle: C1,i,j =(cx, cy, cz)"
  (v3
   (tri-ax-x i)
   (tri-ax-y)
   (tri-ax-z j)))


;;;; The tri-ax-coords function now gives the coordinates of the helix axis for the j-th base pair in the i-th row of the triangle. We have used the v3 function that creates a 3 dimensonal vector. Small provides many functions for operating on vectors and accessing their elements, these can be seen in the TODO: ADD DOC REF file.
;;;; We see that everything works as exected
(tri-ax-coords 1 1)

;;;; Next we define functions to calculate The coordinate location of where the scaffold base joins the helix backbone in the two-dimensional plane of the j th base pair in the i th row in the first triangle. See page p8 of the supplementary info. First we need to calculate the angle that scaffold base makes with the helix's axis. 


(defun theta-1ij (i j &key (odd-offset 0) (even-offset 0))
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


;;;; In defining theta-1ij we have used keyword arguments, this allows us to specify the values we want to use for angular offsets for even and odd rows of helices. Following the paper we use 0 when i in odd, and 180 (pi radians) when i is even, when we call theta-1ij from theta-1ij-scaffold.

(theta-1ij-scaffold 1 1)

;;;; Next we get the coordinate of th

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

;;;; Here we have again used a keyword argument, cm. If cm is t then the returned coordinates will be of the  center of mass of the nucleotide, which is *helix-cm-offset* = 0.6nm from the helixes axis. If cm is nil then the returned coordinates will be given at *helix-radius* = 1nm from the helix axis. The cylindrical->cartesian function is another function provided by small and can be seen in TODO REF FOR cylindrical->cartesian.

;;;; Now its finally time to use these coordinate functions to create the scaffold helices of the triangle. We use the helix-strand function to do this. This function takes as its arugments the coordinate of the 5' end of helixs axis, a unit vector pointing in the 5'->3' direction, a unit vector pointing from the coordinate of the 5' end of helixs axis to the center of mass of the first nucleotide and how many nucleotides the strand contains. It returns a DNA-HELIX-STRAND object with the desired number of nucleotides.

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
    ;TODO: remember to add-prop k later
;;    (break "~A" props)
    hel))



;;;; small allows the heirachial building of objects of increasing levels of abstraction. This allows these objects to be modified and manipulated as individual units. (TODO ADD FIG). In defining (tri-scaffold-helix) we have made use of this without even knowing it. The DNA-HELIX-STRAND class that it returns in on object that represents a strand in a double helix. The DNA strand representation is created from a DNA nucleotides, which are implemented using the DNA-NT class. These DNA-NTs are stored in the children slot of the DNA-HELIX-STRAND.
(children (tri-scaffold-helix 1))

;;;; Similarly the DNA-NTs store their parent in the parent slot. Tracking these relationships allows modifications, such as geometric translations parents, to effect children.
(parent (first (children (tri-scaffold-helix 1))))


;;;; Next we create a function to create the scaffold loops. It takes the helix number, i, and uses the bridging-single-strand to create single strands between the two coordinantes. The first coord specifes the 5' end strand of the strand, the second specifies the 3' coordinate. The last argument is a unit vector that points from the helix axis to the base of the first nucleotide in the strand. bridging-single-strand also takes a keyword argument :len which is the amount of nucleotide to use in the strand. If it is not given the number of nucleotides is automatically calculated based on the distance and the variable TODO *single-nt-length* as given on page 9 of the supplementary information.
(defun tri-scaf-loop (i)
  (let* ((c1 (tri-scaf-coords i (ai i)))
	 (c2 (tri-scaf-coords (+ i 1) (ai (+ i 1))))
	 (loop-strand (bridging-single-strand c1 c2 (v3 0 1 0))))
    ;TODO: (add-prop loop-strand :k k) this somewhere later
    (add-prop loop-strand :i i)
    loop-strand))

(tri-scaf-loop 1)


;;;; We will now create our own higher level of abstraction that represents the dna triangle. To do so we create a class to represent the DNA triangle. Here we make use of the defclass/std package.
(defclass/std dna-triangle2 (dna-origami)
  ((joining-strands :doc "list of joining strands")
   (internal-staps :doc "list of internal staple strands")
   (capping-staps :doc "list of capping staple strands")
   )
  (:documentation "An implementation the DNA a single triangle of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))

;;;; Notice how we extended the dna-origami class. This allows us to make use of the class slots provided by dna-origami and the functions that have been specialized on them. It is good to remember that at any time (describe ...) can be used to gain information on a class or function.

(describe 'dna-origami)

;;;; Now that we have defined our class lets create the dna strands that correspond to the scaffold. 
(defmethod initialize-instance :after ((ori dna-triangle2) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
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
	(3nt ori) (3nt (car (last (scaffold ori))))))

(last (scaffold (make-instance 'dna-triangle2)))
(defparameter tri2 (make-instance 'dna-triangle2))
(wmdna "t2al" (5nt (first (children (make-instance 'dna-triangle2)))))

