;; The first tutorial aims to show how the SIZE dna array of Tikhomirov et al can be created in small.
;; It is dividede into 4 sections, each demonstraiting a section of smalls capabilites
;; In the first section the basic dna model is introduced and some of the api functionality for creating dna is showcased in creating functions to create a triangle of dna tile used in the paper.
;; The next section shows how four such triangles can be used to create the full tile used in the array.
;; Section 4 shows how these tiles are in turn used to create a DNA array, and how staple strands can be extended to create arbitrary pictures.

;;;; First we need to specify that we want to be in the small package to  gain aceess to its functionality
(in-package :small)

;;;; Next we create a class to represent the DNA triangle. Here we make use of the defclass/std package.
(defclass/std dna-triangle2 (dna-origami)
  ((joining-strands :doc "list of joining strands")
   (internal-staps :doc "list of internal staple strands")
   (capping-staps :doc "list of capping staple strands")
   )
  (:documentation "An implementation the DNA a single triangle of the tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256. The triangle has coords which correspond to index k=1 with the y-coords flipped to make the axis correspond to normal cartesian coords"))

;;;; Notice how we extended the dna-origami class. This allows us to make use of the class slots provided by dna-origami and the functions that have been specialized on them. It is good to remember that at any time (describe ...) can be used to gain information on a class or function.

(describe 'dna-origami)

;;;; Next we introduce the constants as set out in Tikhomirov et al. See pages 7-11 of the supplementary information for information on the geometric model. If you are unfamilair with common lisp the it uses infix notation. Code is composed of list, with the start denoted by ( and the end donted by ). The first argument after the opening of a list is the function name to be called. In the following codesnippets it is defparameter, which binds sybols to values. The list items that follow are the arguments to the function. In this case the fist argument is the symbol to bind and the following arugment is value to bind it to. Lisp also the ability code to add documentation to itself. The third argument is a docstring that adds such documentation describing the introduced symbols. The surrounding *'s in the name of the variable is a convention in common lisp for denoting global variables.


(defparameter *i1*  11.3d0
  "The length of the shortest double helix in the square")


     
(defparameter *r*  11
  "the total number of rows with increasing length in each of the four isosceles right triangles composing the square")

(defparameter *2r*  (* 2 *r*)
  "the total number of rows with increasing length in each of the four isosceles right triangles composing the square")

(defparameter *g* 1.42d0 "Distance between the center of the square to the central vertex of each of the four triangles")


(defparameter *w* (+ (* 2 (+ *i1* *g*))
		     (* (+ *helix-diameter* *helix-spacing*)
			(- (* 2 *r*)
			   1))))

 ;;;; Next we define a function to calculate the number of base pairs in each row of the double helix. Notice how we use the sybols *helix-diameter* *helix-spacing* and *helix-nt-spacing*. These are variable provided by small (see the dna.lisp file) that define common parameters of DNA.
(defun ai (i)
  (if (or (> i *2r*) (< i 0))
      (error "~a is an invalid index for ai calculation. valid indices: [1, ~a]" i 2r)
      (if (<= i *r*)
	  (round (/ (+ *i1*
		       (* (+ *helix-diameter* *helix-spacing*)
			  (- i 1)))
		    *helix-nt-spacing*))
	  (ai (+ 2r 1 (- i))))))


(defun tri-ax-x (i)
  "Calculate the x coordinate (left-right) of the helix axis in the two-dimensional plane of the  bases pair in the i-th row in the 'top' triangle [0-3] going clockwise with 0 at top
Returns: float (x with)
Note: The geometric model in https://www.nature.com/articles/nnano.2016.256 defines the coordinate system"
  (float (+ (- (/ *w* 2))
	    *i1*
	    *g*
	    (* (+ *helix-diameter* *helix-spacing*)
	       (- i 1)))))

(helix-axis-x-coord-1 1)

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

(defun theta-1ij-scaffold (i j)
  (theta-1ij i j
	     :odd-offset 0
	     :even-offset pi))


(defun tri-scaf-coords (i j &key cm)
  "The coordinate location of where the scaffold base joins the backbone in the two-dimensional plane of the j th base pair in the i th row in the first triangle"
  (let* ((helix-axis (tri-ax-coords i j))
	 (theta (theta-1ij-scaffold i j))
	 (cyl-vec (if cm
		      (v3 *helix-cm-offset* theta 0) ;; adjustment helix cm.
		      (v3 1 theta 0)))     ; helix rad/bb cord = 1nm
	 (cart-cyl (cylindrical->cartesian cyl-vec))
	 (coords (.+ helix-axis cart-cyl)))
    coords))


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
    ;TODO: remember to add k later
;;    (break "~A" props)
    hel))

(defmethod initialize-instance :after ((ori dna-triangle2) &key)  
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (loop for i from 1 to 22 do
      (progn
	;;(break "scaff ~A" (scaffold ori))
	(add-to-scaffold ori (tri-scaffold-helix i))
	(when (evenp i)
	  (unless (= *2r* i)
	    (add-to-scaffold ori (SMALL::scaffold-loop 1 i)))
	  )))
  )

(defparameter t2 (make-instance 'dna-triangle2))
(wmdna "t2al" (5nt (first (children (make-instance 'dna-triangle2)))))



					;(break ori)
    (mapcar #'(lambda (nt base)
		(with-accessors ((cm cm) (vbb vbb) (vn vn)) nt  
		  (update-base nt  base) ;Set the bases to match the m13 seq
		  ;; Update coords since we want regular carteisan
		  ;;and the paper defines y in the opposite direction
		  (setf (cm nt) (@ (from-diag '(1d0 -1d0 1d0)) cm)) 
		  (setf (vbb nt) (@ (from-diag '(1d0 -1d0 1d0)) vbb))
		  (setf (vn nt) (@ (from-diag '(1d0 -1d0 1d0)) vn))))
	    (connected-nts (5nt (first (scaffold ori))))				
	    (map 'list #'string  *m13mp18*))

    (setf (5nt ori) (5nt (find-obj-with-props (scaffold ori)
					      `((:i . 1) (:k . 1)))))
    (setf (3nt ori) (3nt (find-obj-with-props (scaffold ori)
					      `((:i . 22) (:k . 1)))))
    ;; Now we add  staples to hold this bad boy together. awwww yeah
    (setf (internal-staps ori) (internal-staples ori))
    (push (u-staples-tri ori) (internal-staps ori))
    (mapcar #'(lambda (stap)
		(add-parent stap ori))
	    (alexandria:flatten (internal-staps ori)))
    ;; These are cappping end added in a hacky way
    
    (setf (capping-staps ori) (capping-ends ori));; (capping-ends ori :indices '(1 3 5 7 9 11 13 15 17 19 21))
    (mapcar #'(lambda (stap)
		(add-parent stap ori))
	    (capping-staps ori))
	    
    ori)
