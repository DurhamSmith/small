(in-package :small)


(defclass/std dna-tile (dna)
  ()
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

(defun make-dna-tile (&key tfms)
  "Creates an instance of a tile object"
  (make-instance 'dna-tile :tfms tfms))


(defmethod initialize-instance :after ((tile dna-tile) &key)
  "Creates DNA origamis scaffold subobjs (scaff helices, loops, bridges), joins them together to form them scaffold strand and updates their base seq. Then creates origamis staples and staple bridges. Saves these to the instance of the object"
  ;; (loop for k from 1 to 4 collect
  ;; 			  ((add-to-scaff (create-triangle tile k))
  ;; 			   (when (between k 1 3)
  ;; 			     (add-to-scaff (scaff-bridge k)))))
  ;; (add-staples (create-staple-briges))
  )


(defmethod create-staple-bridges (tile dna-tile)
  "Creates all the staple bridges to connect triangle 1-4 together. Returns a list of DNA-SINGLE-STRAND's")

(describe #'create-triangle)
(defun create-triangle (&opt (k 1))
  "Returns a DNA-TILE-TRIANGLE CHEM-OBJ correctly rotated for the position of its index, k"
  ;(make-dna-tile-triangle :tfms equal)
)

(defmethod scaff-bridge ((tile dna-tile) k &key num-nts)
  "Create and returns a DNA-SINGLE-STRAND which originates at NUCLEOTIDE ai of DNA-TILE-TRIANGLE k's 2r-th helixes scaffold DNA-SINGLE (SC_{k,2r,a_{2r}}) and ends at (SC_{(k+1),1,a_1}). num-nts nuclotides are added, if num-nts are not specified then $\frac{euclidean dist}{single-nt-len}"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dna-tile-triangle class. A composite chem-obj used in the construction of dna-tile 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-tile-triangle (dna-origami)
  (:documentation "A DNA-ORIGAMI CHEM-OBJ that contains the scaffold strand (including scaff loops, excluding scaff bridges) and staple strands (excluding those that that also form staple bridges)"))

(defun make-dna-tile-triangle (&key tfms)
  (make-instance 'dna-tile-triangle :tfms tfms))

(defmethod initialize-instance :after ((tri dna-tile-triangle) &key)
  "Create the dna-origami chem-objs that represent the scaffold strand (scaff helixes = 2r = 22, scaff loops = 21, scaff bridges not included) and staple strands (TODO NUMBER, this excludes staples that also form staple bridges)"
  ;; 1: Create scaff helix
  ;;; Get starting positions of the bases on the 5' and 3' end of helix i
  ;;; Get vec for 5-3 dir of helix axis
  ;;; Get vbb = theta on page 8 of the papers sup info
  ;;; Create a double helix strand and add it to scaff-subobjs
  ;; 2: Create scaff loops
  ;;; vaxis points from helix i's last scaff nt (3' end) and helix i+1's first scaff nt (5' end)
  ;;; vbb is perp to vaxis and points 'kind of' (dotproduct is +) in helix i's axis 5'->3' direction
  ;;; nts are calculated based on single nt len and distance needed to be spanned
  ;; 3: Create staple stands
  ;;; (staple tri ((:start :end) (:connect-by :strand) (:start :end) (:connect-by :strand) (:start :end)))
  )

(defparameter *i1*  11.3d0
  "The length of the shortest double helix in the square")

     
(defparameter *r*  11
  "the total number of rows with increasing length in each of the four isosceles right triangles composing the square")

(defparameter *g* 1.42d0 "Distance between the center of the square to the central vertex of each of the four triangles")


(defparameter *w* (+ (* 2 (+ *i1* *g*))
		     (* (+ *helix-diameter* *helix-spacing*)
			(- (* 2 *r*)
			   1))))

(defun ai (i)
  (let ((2r (* 2 *r*)))
    (if (or (> i 2r) (< i 0))
	(error "~a is an invalid index for ai calculation. valid indices: [1, ~a]" i 2r)
	(if (<= i *r*)
	    (round (/ (+ **i1**
			 (* (+ *helix-diameter* *helix-spacing*)
			    (- i 1)))
		      *helix-nt-spacing*))
	    (ai (+ 2r 1 (- i)))))))




(defun helix-axis-x-coord-1 (i)
  "Calculate the x coordinate (left-right) of the helix axis in the two-dimensional plane of the  bases pair in the i-th row in the 'top' triangle [0-3] going clockwise with 0 at top
Returns: float (x with)
Note: The geometric model in https://www.nature.com/articles/nnano.2016.256 defines the coordinate system"
  (float (+ (- (/ *w* 2))
	    *i1*
	    *g*
	    (* (+ *helix-diameter* *helix-spacing*)
	       (- i 1)))))

(helix-axis-x-coord-1 1)

(defun helix-axis-y-coord-1 ()
  "Calculate the y coordinate (in/out) in the 'top' triangle [0-3] going clockwise with 0 at top
Returns: float (y)
Note: The geometric model inhttps://www.nature.com/articles/nnano.2016.256 defines the coordinate system and y is constant at 0 for the helix axis"
  0d0)

(defun helix-axis-z-coord-1 (j)
  "Calculate the z coordinate (up/down) of the helix axis in the two-dimensional plane of the  
j-th base pair in a row of the 'top' triangle triangle [0-3] going clockwise with 0 at top
Returns: float (y)
Note: The geometric model inhttps://www.nature.com/articles/nnano.2016.256 defines the coordinate system"
    (float (+ (- (/ *w* 2))
	      (* *helix-nt-spacing* j))))
  


(helix-axis-coords-1 1 1)
(defun helix-axis-coords-1 (i j)
  "The coordinate location of the helix axis in the two-dimensional plane of the j th base pair in the i th row in the first triangle: C1,i,j =(cx, cy, cz)"
  (v3
   (helix-axis-x-coord-1 i)
   (helix-axis-y-coord-1)
   (helix-axis-z-coord-1 j)))


(defun helix-axis-coords-1 (i j)
  "The coordinate location of the helix axis in the two-dimensional plane of the j th base pair in the i th row in the first triangle: C1,i,j =(cx, cy, cz)"
  (v3
   (helix-axis-x-coord-1 i)
   (helix-axis-y-coord-1)
   (helix-axis-z-coord-1 j)))



(defmethod helix-axis-coords (k i j)
  "Returns the coords for the staple in triangle. tile: a DNA tile object k: triangle index [1-4] clockwise starting at the top j: j-th base pair i: i th row Returns: VECTOR/DOUBLE-FLOAT (magicl) of the staple coordinates"
  (if (eql k 1)
      (helix-axis-coords-1 i j)
      (rotate-vec (helix-axis-coords (- k 1) i j)   ;reccursive rotate around xz axis
		  (v3 0 1 0)
		  (/ pi 2))))


(theta-1ij 1 3 )
(defmethod theta-1ij (i j &key (odd-offset 0) (even-offset 0))
  (let* ((rotation (mod (* (- j 1)
			   *rad/bp*)
			(* 2 pi)))
	 (theta (if (oddp i)
		    (mod (+ odd-offset rotation) (* 2 pi))
		    (mod (+ even-offset rotation) (* 2 pi)))))
    theta))


(defmethod theta-1ij-scaffold (i j)
  (theta-1ij i j
	     :odd-offset 0
	     :even-offset (/ pi 2)))

(defmethod theta-1ij-staple (i j)
  (theta-1ij i j
	     :odd-offset (- (* 150 (/ 1 pi)))  ;150deg=>rad
	     :even-offset (+ 180 (* 150 (/ 1 pi)))))


