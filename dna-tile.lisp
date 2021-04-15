(in-package :small)

(defparameter *m13mp18*  "AATGCTACTACTATTAGTAGAATTGATGCCACCTTTTCAGCTCGCGCCCCAAATGAAAATATAGCTAAACAGGTTATTGACCATTTGCGAAATGTATCTAATGGTCAAACTAAATCTACTCGTTCGCAGAATTGGGAATCAACTGTTATATGGAATGAAACTTCCAGACACCGTACTTTAGTTGCATATTTAAAACATGTTGAGCTACAGCATTATATTCAGCAATTAAGCTCTAAGCCATCCGCAAAAATGACCTCTTATCAAAAGGAGCAATTAAAGGTACTCTCTAATCCTGACCTGTTGGAGTTTGCTTCCGGTCTGGTTCGCTTTGAAGCTCGAATTAAAACGCGATATTTGAAGTCTTTCGGGCTTCCTCTTAATCTTTTTGATGCAATCCGCTTTGCTTCTGACTATAATAGTCAGGGTAAAGACCTGATTTTTGATTTATGGTCATTCTCGTTTTCTGAACTGTTTAAAGCATTTGAGGGGGATTCAATGAATATTTATGACGATTCCGCAGTATTGGACGCTATCCAGTCTAAACATTTTACTATTACCCCCTCTGGCAAAACTTCTTTTGCAAAAGCCTCTCGCTATTTTGGTTTTTATCGTCGTCTGGTAAACGAGGGTTATGATAGTGTTGCTCTTACTATGCCTCGTAATTCCTTTTGGCGTTATGTATCTGCATTAGTTGAATGTGGTATTCCTAAATCTCAACTGATGAATCTTTCTACCTGTAATAATGTTGTTCCGTTAGTTCGTTTTATTAACGTAGATTTTTCTTCCCAACGTCCTGACTGGTATAATGAGCCAGTTCTTAAAATCGCATAAGGTAATTCACAATGATTAAAGTTGAAATTAAACCATCTCAAGCCCAATTTACTACTCGTTCTGGTGTTTCTCGTCAGGGCAAGCCTTATTCACTGAATGAGCAGCTTTGTTACGTTGATTTGGGTAATGAATATCCGGTTCTTGTCAAGATTACTCTTGATGAAGGTCAGCCAGCCTATGCGCCTGGTCTGTACACCGTTCATCTGTCCTCTTTCAAAGTTGGTCAGTTCGGTTCCCTTATGATTGACCGTCTGCGCCTCGTTCCGGCTAAGTAACATGGAGCAGGTCGCGGATTTCGACACAATTTATCAGGCGATGATACAAATCTCCGTTGTACTTTGTTTCGCGCTTGGTATAATCGCTGGGGGTCAAAGATGAGTGTTTTAGTGTATTCTTTTGCCTCTTTCGTTTTAGGTTGGTGCCTTCGTAGTGGCATTACGTATTTTACCCGTTTAATGGAAACTTCCTCATGAAAAAGTCTTTAGTCCTCAAAGCCTCTGTAGCCGTTGCTACCCTCGTTCCGATGCTGTCTTTCGCTGCTGAGGGTGACGATCCCGCAAAAGCGGCCTTTAACTCCCTGCAAGCCTCAGCGACCGAATATATCGGTTATGCGTGGGCGATGGTTGTTGTCATTGTCGGCGCAACTATCGGTATCAAGCTGTTTAAGAAATTCACCTCGAAAGCAAGCTGATAAACCGATACAATTAAAGGCTCCTTTTGGAGCCTTTTTTTTGGAGATTTTCAACGTGAAAAAATTATTATTCGCAATTCCTTTAGTTGTTCCTTTCTATTCTCACTCCGCTGAAACTGTTGAAAGTTGTTTAGCAAAATCCCATACAGAAAATTCATTTACTAACGTCTGGAAAGACGACAAAACTTTAGATCGTTACGCTAACTATGAGGGCTGTCTGTGGAATGCTACAGGCGTTGTAGTTTGTACTGGTGACGAAACTCAGTGTTACGGTACATGGGTTCCTATTGGGCTTGCTATCCCTGAAAATGAGGGTGGTGGCTCTGAGGGTGGCGGTTCTGAGGGTGGCGGTTCTGAGGGTGGCGGTACTAAACCTCCTGAGTACGGTGATACACCTATTCCGGGCTATACTTATATCAACCCTCTCGACGGCACTTATCCGCCTGGTACTGAGCAAAACCCCGCTAATCCTAATCCTTCTCTTGAGGAGTCTCAGCCTCTTAATACTTTCATGTTTCAGAATAATAGGTTCCGAAATAGGCAGGGGGCATTAACTGTTTATACGGGCACTGTTACTCAAGGCACTGACCCCGTTAAAACTTATTACCAGTACACTCCTGTATCATCAAAAGCCATGTATGACGCTTACTGGAACGGTAAATTCAGAGACTGCGCTTTCCATTCTGGCTTTAATGAGGATTTATTTGTTTGTGAATATCAAGGCCAATCGTCTGACCTGCCTCAACCTCCTGTCAATGCTGGCGGCGGCTCTGGTGGTGGTTCTGGTGGCGGCTCTGAGGGTGGTGGCTCTGAGGGTGGCGGTTCTGAGGGTGGCGGCTCTGAGGGAGGCGGTTCCGGTGGTGGCTCTGGTTCCGGTGATTTTGATTATGAAAAGATGGCAAACGCTAATAAGGGGGCTATGACCGAAAATGCCGATGAAAACGCGCTACAGTCTGACGCTAAAGGCAAACTTGATTCTGTCGCTACTGATTACGGTGCTGCTATCGATGGTTTCATTGGTGACGTTTCCGGCCTTGCTAATGGTAATGGTGCTACTGGTGATTTTGCTGGCTCTAATTCCCAAATGGCTCAAGTCGGTGACGGTGATAATTCACCTTTAATGAATAATTTCCGTCAATATTTACCTTCCCTCCCTCAATCGGTTGAATGTCGCCCTTTTGTCTTTGGCGCTGGTAAACCATATGAATTTTCTATTGATTGTGACAAAATAAACTTATTCCGTGGTGTCTTTGCGTTTCTTTTATATGTTGCCACCTTTATGTATGTATTTTCTACGTTTGCTAACATACTGCGTAATAAGGAGTCTTAATCATGCCAGTTCTTTTGGGTATTCCGTTATTATTGCGTTTCCTCGGTTTCCTTCTGGTAACTTTGTTCGGCTATCTGCTTACTTTTCTTAAAAAGGGCTTCGGTAAGATAGCTATTGCTATTTCATTGTTTCTTGCTCTTATTATTGGGCTTAACTCAATTCTTGTGGGTTATCTCTCTGATATTAGCGCTCAATTACCCTCTGACTTTGTTCAGGGTGTTCAGTTAATTCTCCCGTCTAATGCGCTTCCCTGTTTTTATGTTATTCTCTCTGTAAAGGCTGCTATTTTCATTTTTGACGTTAAACAAAAAATCGTTTCTTATTTGGATTGGGATAAATAATATGGCTGTTTATTTTGTAACTGGCAAATTAGGCTCTGGAAAGACGCTCGTTAGCGTTGGTAAGATTCAGGATAAAATTGTAGCTGGGTGCAAAATAGCAACTAATCTTGATTTAAGGCTTCAAAACCTCCCGCAAGTCGGGAGGTTCGCTAAAACGCCTCGCGTTCTTAGAATACCGGATAAGCCTTCTATATCTGATTTGCTTGCTATTGGGCGCGGTAATGATTCCTACGATGAAAATAAAAACGGCTTGCTTGTTCTCGATGAGTGCGGTACTTGGTTTAATACCCGTTCTTGGAATGATAAGGAAAGACAGCCGATTATTGATTGGTTTCTACATGCTCGTAAATTAGGATGGGATATTATTTTTCTTGTTCAGGACTTATCTATTGTTGATAAACAGGCGCGTTCTGCATTAGCTGAACATGTTGTTTATTGTCGTCGTCTGGACAGAATTACTTTACCTTTTGTCGGTACTTTATATTCTCTTATTACTGGCTCGAAAATGCCTCTGCCTAAATTACATGTTGGCGTTGTTAAATATGGCGATTCTCAATTAAGCCCTACTGTTGAGCGTTGGCTTTATACTGGTAAGAATTTGTATAACGCATATGATACTAAACAGGCTTTTTCTAGTAATTATGATTCCGGTGTTTATTCTTATTTAACGCCTTATTTATCACACGGTCGGTATTTCAAACCATTAAATTTAGGTCAGAAGATGAAATTAACTAAAATATATTTGAAAAAGTTTTCTCGCGTTCTTTGTCTTGCGATTGGATTTGCATCAGCATTTACATATAGTTATATAACCCAACCTAAGCCGGAGGTTAAAAAGGTAGTCTCTCAGACCTATGATTTTGATAAATTCACTATTGACTCTTCTCAGCGTCTTAATCTAAGCTATCGCTATGTTTTCAAGGATTCTAAGGGAAAATTAATTAATAGCGACGATTTACAGAAGCAAGGTTATTCACTCACATATATTGATTTATGTACTGTTTCCATTAAAAAAGGTAATTCAAATGAAATTGTTAAATGTAATTAATTTTGTTTTCTTGATGTTTGTTTCATCATCTTCTTTTGCTCAGGTAATTGAAATGAATAATTCGCCTCTGCGCGATTTTGTAACTTGGTATTCAAAGCAATCAGGCGAATCCGTTATTGTTTCTCCCGATGTAAAAGGTACTGTTACTGTATATTCATCTGACGTTAAACCTGAAAATCTACGCAATTTCTTTATTTCTGTTTTACGTGCAAATAATTTTGATATGGTAGGTTCTAACCCTTCCATTATTCAGAAGTATAATCCAAACAATCAGGATTATATTGATGAATTGCCATCATCTGATAATCAGGAATATGATGATAATTCCGCTCCTTCTGGTGGTTTCTTTGTTCCGCAAAATGATAATGTTACTCAAACTTTTAAAATTAATAACGTTCGGGCAAAGGATTTAATACGAGTTGTCGAATTGTTTGTAAAGTCTAATACTTCTAAATCCTCAAATGTATTATCTATTGACGGCTCTAATCTATTAGTTGTTAGTGCTCCTAAAGATATTTTAGATAACCTTCCTCAATTCCTTTCAACTGTTGATTTGCCAACTGACCAGATATTGATTGAGGGTTTGATATTTGAGGTTCAGCAAGGTGATGCTTTAGATTTTTCATTTGCTGCTGGCTCTCAGCGTGGCACTGTTGCAGGCGGTGTTAATACTGACCGCCTCACCTCTGTTTTATCTTCTGCTGGTGGTTCGTTCGGTATTTTTAATGGCGATGTTTTAGGGCTATCAGTTCGCGCATTAAAGACTAATAGCCATTCAAAAATATTGTCTGTGCCACGTATTCTTACGCTTTCAGGTCAGAAGGGTTCTATCTCTGTTGGCCAGAATGTCCCTTTTATTACTGGTCGTGTGACTGGTGAATCTGCCAATGTAAATAATCCATTTCAGACGATTGAGCGTCAAAATGTAGGTATTTCCATGAGCGTTTTTCCTGTTGCAATGGCTGGCGGTAATATTGTTCTGGATATTACCAGCAAGGCCGATAGTTTGAGTTCTTCTACTCAGGCAAGTGATGTTATTACTAATCAAAGAAGTATTGCTACAACGGTTAATTTGCGTGATGGACAGACTCTTTTACTCGGTGGCCTCACTGATTATAAAAACACTTCTCAGGATTCTGGCGTACCGTTCCTGTCTAAAATCCCTTTAATCGGCCTCCTGTTTAGCTCCCGCTCTGATTCTAACGAGGAAAGCACGTTATACGTGCTCGTCAAAGCAACCATAGTACGCGCCCTGTAGCGGCGCATTAAGCGCGGCGGGTGTGGTGGTTACGCGCAGCGTGACCGCTACACTTGCCAGCGCCCTAGCGCCCGCTCCTTTCGCTTTCTTCCCTTCCTTTCTCGCCACGTTCGCCGGCTTTCCCCGTCAAGCTCTAAATCGGGGGCTCCCTTTAGGGTTCCGATTTAGTGCTTTACGGCACCTCGACCCCAAAAAACTTGATTTGGGTGATGGTTCACGTAGTGGGCCATCGCCCTGATAGACGGTTTTTCGCCCTTTGACGTTGGAGTCCACGTTCTTTAATAGTGGACTCTTGTTCCAAACTGGAACAACACTCAACCCTATCTCGGGCTATTCTTTTGATTTATAAGGGATTTTGCCGATTTCGGAACCACCATCAAACAGGATTTTCGCCTGCTGGGGCAAACCAGCGTGGACCGCTTGCTGCAACTCTCTCAGGGCCAGGCGGTGAAGGGCAATCAGCTGTTGCCCGTCTCACTGGTGAAAAGAAAAACCACCCTGGCGCCCAATACGCAAACCGCCTCTCCCCGCGCGTTGGCCGATTCATTAATGCAGCTGGCACGACAGGTTTCCCGACTGGAAAGCGGGCAGTGAGCGCAACGCAATTAATGTGAGTTAGCTCACTCATTAGGCACCCCAGGCTTTACACTTTATGCTTCCGGCTCGTATGTTGTGTGGAATTGTGAGCGGATAACAATTTCACACAGGAAACAGCTATGACCATGATTACGAATTCGAGCTCGGTACCCGGGGATCCTCTAGAGTCGACCTGCAGGCATGCAAGCTTGGCACTGGCCGTCGTTTTACAACGTCGTGACTGGGAAAACCCTGGCGTTACCCAACTTAATCGCCTTGCAGCACATCCCCCTTTCGCCAGCTGGCGTAATAGCGAAGAGGCCCGCACCGATCGCCCTTCCCAACAGTTGCGCAGCCTGAATGGCGAATGGCGCTTTGCCTGGTTTCCGGCACCAGAAGCGGTGCCGGAAAGCTGGCTGGAGTGCGATCTTCCTGAGGCCGATACTGTCGTCGTCCCCTCAAACTGGCAGATGCACGGTTACGATGCGCCCATCTACACCAACGTGACCTATCCCATTACGGTCAATCCGCCGTTTGTTCCCACGGAGAATCCGACGGGTTGTTACTCGCTCACATTTAATGTTGATGAAAGCTGGCTACAGGAAGGCCAGACGCGAATTATTTTTGATGGCGTTCCTATTGGTTAAAAAATGAGCTGATTTAACAAAAATTTAATGCGAATTTTAACAAAATATTAACGTTTACAATTTAAATATTTGCTTATACAATCTTCCTGTTTTTGGGGCTTTTCTGATTATCAACCGGGGTACATATGATTGACATGCTAGTTTTACGATTACCGTTCATCGATTCTCTTGTTTGCTCCAGACTCTCAGGCAATGACCTGATAGCCTTTGTAGATCTCTCAAAAATAGCTACCCTCTCCGGCATTAATTTATCAGCTAGAACGGTTGAATATCATATTGATGGTGATTTGACTGTCTCCGGCCTTTCTCACCCTTTTGAATCTTTACCTACACATTACTCAGGCATTGCATTTAAAATATATGAGGGTTCTAAAAATTTTTATCCTTGCGTTGAAATAAAGGCTTCTCCCGCAAAAGTATTACAGGGTCATAATGTTTTTGGTACAACCGATTTAGCTTTATGCTCTGAGGCTTTATTGCTTAATTTTGCTAATTCTTTGCCTTGCCTGTATGATTTATTGGATGTT")

(defclass/std dna-tile (dna-origami)
  ((stap-bridges :doc "stap bridges")
   (int-staps :doc " bridges")
   (u-staps :doc "stap bridges"))
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

(defun make-dna-tile ()
  "Creates an instance of a tile object"
  (make-instance 'dna-tile))



;; (defmethod initialize-instance :after ((tile dna-tile) &key)
;;   "Creates DNA origamis scaffold subobjs (scaff helices, loops, bridges), joins them together to form them scaffold strand and updates their base seq. Then creates origamis staples and staple bridges. Saves these to the instance of the object"
;;   ;; (loop for k from 1 to 4 collect
;;   ;; 			  ((add-to-scaff (create-triangle tile k))
;;   ;; 			   (when (between k 1 3)
;;   ;; 			     (add-to-scaff (scaff-bridge k)))))
;;   ;; (add-staples (create-staple-briges))
;;   )


;; (defmethod create-staple-bridges (tile dna-tile)
;;   "Creates all the staple bridges to connect triangle 1-4 together. Returns a list of DNA-SINGLE-STRAND's")

;; (describe #'create-triangle)
;; (defun create-triangle ((k 1))
;;   "Returns a DNA-TILE-TRIANGLE CHEM-OBJ correctly rotated for the position of its index, k"
;;   ;(make-dna-tile-triangle :tfms equal)
;; )

;; (defmethod scaff-bridge ((tile dna-tile) k &key num-nts)
;;   "Create and returns a DNA-SINGLE-STRAND which originates at NUCLEOTIDE ai of DNA-TILE-TRIANGLE k's 2r-th helixes scaffold DNA-SINGLE (SC_{k,2r,a_{2r}}) and ends at (SC_{(k+1),1,a_1}). num-nts nuclotides are added, if num-nts are not specified then $\frac{euclidean dist}{single-nt-len}"
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dna-tile-triangle class. A composite chem-obj used in the construction of dna-tile 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defclass/std dna-tile-triangle (dna-origami)
;;   (:documentation "A DNA-ORIGAMI CHEM-OBJ that contains the scaffold strand (including scaff loops, excluding scaff bridges) and staple strands (excluding those that that also form staple bridges)"))

;; (defun make-dna-tile-triangle (&key tfms)
;;   (make-instance 'dna-tile-triangle :tfms tfms))

;; (defmethod initialize-instance :after ((tri dna-tile-triangle) &key)
;;   "Create the dna-origami chem-objs that represent the scaffold strand (scaff helixes = 2r = 22, scaff loops = 21, scaff bridges not included) and staple strands (TODO NUMBER, this excludes staples that also form staple bridges)"
;;   ;; 1: Create scaff helix
;;   ;;; Get starting positions of the bases on the 5' and 3' end of helix i
;;   ;;; Get vec for 5-3 dir of helix axis
;;   ;;; Get vbb = theta on page 8 of the papers sup info
;;   ;;; Create a double helix strand and add it to scaff-subobjs
;;   ;; 2: Create scaff loops
;;   ;;; vaxis points from helix i's last scaff nt (3' end) and helix i+1's first scaff nt (5' end)
;;   ;;; vbb is perp to vaxis and points 'kind of' (dotproduct is +) in helix i's axis 5'->3' direction
;;   ;;; nts are calculated based on single nt len and distance needed to be spanned
;;   ;; 3: Create staple stands
;;   ;;; (staple tri ((:start :end) (:connect-by :strand) (:start :end) (:connect-by :strand) (:start :end)))
;;   )

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

(defun ai (i)
  (let ((2r (* 2 *r*)))
    (if (or (> i 2r) (< i 0))
	(error "~a is an invalid index for ai calculation. valid indices: [1, ~a]" i 2r)
	(if (<= i *r*)
	    (round (/ (+ *i1*
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
  


(defun helix-axis-coords-1 (i j)
  "The coordinate location of the helix axis in the two-dimensional plane of the j th base pair in the i th row in the first triangle: C1,i,j =(cx, cy, cz)"
  (v3
   (helix-axis-x-coord-1 i)
   (helix-axis-y-coord-1)
   (helix-axis-z-coord-1 j)))



(defun helix-axis-coords (k i j)
  "Returns the coords for the staple in triangle. tile: a DNA tile object k: triangle index [1-4] clockwise starting at the top j: j-th base pair i: i th row Returns: VECTOR/DOUBLE-FLOAT (magicl) of the staple coordinates"
  (if (eql k 1)
      (helix-axis-coords-1 i j)
      (rotate-vec (helix-axis-coords (- k 1) i j)   ;reccursive rotate around xz axis
		  (v3 0 -1 0) ;;use -1 for z axis since they define axis this way in paper
		  (/ pi 2))))



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



(defun theta-1ij-staple (i j)
  (theta-1ij i j
	     :odd-offset (deg->rad -150)
	     :even-offset (deg->rad (+ 180 150))))


(defun scaffold-coords-1 (i j &key cm)
  "The coordinate location of where the scaffold base joins the backbone in the two-dimensional plane of the j th base pair in the i th row in the first triangle"
  (let* ((helix-axis (helix-axis-coords-1 i j))
	 (theta (theta-1ij-scaffold i j))
	 (cyl-vec (if cm
		      (v3 *helix-cm-offset* theta 0) ;; adjustment helix cm.
		      (v3 1 theta 0)))     ; helix rad/bb cord = 1nm
	 (cart-cyl (cylindrical->cartesian cyl-vec))
	 (coords (.+ helix-axis cart-cyl)))
    coords))

(cylindrical->cartesian (v3 1 pi 0))

(defun staple-coords-1 (i j &key cm)
  "The coordinate location of where the scaffold base joins the backbone in the two-dimensional plane of the j th base pair in the i th row in the first triangle"
  (let* ((helix-axis (helix-axis-coords-1  i j))
	 (theta (theta-1ij-staple  i j))
	 (cyl-vec (if cm
		      (v3 *helix-cm-offset* theta 0) ;; adjustment helix cm.
		      (v3 1 theta 0)))     ; helix rad/bb cord = 1nm
	 (cart-cyl (cylindrical->cartesian cyl-vec))
	 (coords (.+ helix-axis cart-cyl)))
    coords))


(defun staple-coords (k i j  &key cm)
  "Returns the coords for the staple in triangle. tile: a DNA tile object k: triangle index [1-4] clockwise starting at the top j: j-th base pair i: i th row Returns: VECTOR/DOUBLE-FLOAT (magicl) of the staple coordinates"
  (if (eql k 1)
      (staple-coords-1  i j :cm cm)
      (rotate-vec (staple-coords (- k 1) i j) (v3 0 -1 0) (/ pi 2))))


(defun scaffold-coords (k i j &key cm)
  "Returns the coords for the scaffold in triangle. tile: a DNA tile object k: triangle index [1-4] clockwise starting at the top j: j-th base pair i: i th row Returns: VECTOR/DOUBLE-FLOAT (magicl) of the scaffold coordinates"
  (if (eql k 1)
      (scaffold-coords-1 i j :cm cm)
      (rotate-vec (scaffold-coords (- k 1) i j :cm cm) (v3 0 -1 0) (/ pi 2)))) ;; use -1 for z axis since they define axis this way in paper

(scaffold-coords 1 1 33 :cm nil)
(scaffold-coords 1 22 33 :cm nil)
(scaffold-coords 2 1 33 :cm nil)

(defun scaffold-helix (k i)
  (let* ((j (if (oddp i)
		(ai i)
		1))
	 (5axis (if (oddp i)
		    (helix-axis-coords k i j)
		    (helix-axis-coords k i 1)))
	 (3axis (if (oddp i)		    
		    (helix-axis-coords k i 1)
		    (helix-axis-coords k i (ai i))))
	 (vn (as-unit-vec (.- 3axis 5axis)))
	 (cm (scaffold-coords k i j :cm t))
	 (vbb0 (as-unit-vec (.- cm 5axis)))
	 (hel (helix-strand 5axis vn vbb0 (ai i))))
    (add-prop hel :k k)
    (add-prop hel :i i)
;;    (break "~A" props)
    hel))

(defun scaffold-loop (k i)
  (let* ((c1 (scaffold-coords k i (ai i)))
	 (c2 (if (= i (* 2 *r*))
		 (scaffold-coords (+ k 1) 1 (ai 1))
		 (scaffold-coords k (+ i 1) (ai (+ i 1)))))
	 (loop-strand (bridging-single-strand c1 c2 (v3 0 1 0))))
    (add-prop loop-strand :k k)
    (add-prop loop-strand :i i)
    loop-strand))
	 




;; (defmethod edge-staple (h1 h2)
;;   "Creates the edge staples that hold helix h1 and h2 together"
;;   (let* ((staps (create-staple
;; 		 `((:obj ,h1  :start 0 :end 16  :from-3end t)
;; 		   (:obj ,h2  :start 0 :end 16  :from-3end nil))))
;; 	 (staple-strand (staple-from-objs staps)))
;;     staple-strand))



(defmethod edge-staple ((tile dna-tile) k i sc-hel1 sc-hel2)
  "Creates the edge staples that hold helix sc-hel1 and sc-hel2 together Goes from 5-3 dir"
  (when (or (= k *2r*) (oddp i))
    (error "Index not supported k: ~A i: ~A" k i))
  
  (let* ((staps (create-staple
		 `((:obj ,sc-hel1  :start 0 :end 16  :from-3end t)
		   (:obj ,sc-hel2  :start 0 :end 16  :from-3end nil))))
	 (staple-strand staps)) ;(staple-from-objs staps)))
    (mapcar #'(lambda (x)
		(add-parent x staple-strand))
	    staps)
    staple-strand))



;(break (make-dna-tile))
(defmethod initialize-instance :after ((ori dna-tile) &key)
  "Create the dna-origami chem-objs that represent the scaffold strand (scaff helixes = 2r = 22, scaff loops = 21, scaff bridges not included) and staple strands (TODO NUMBER, this excludes staples that also form staple bridges)"
  (with-accessors ((scaff scaffold)) ori
    ;;Fist we loop over the scaffold so that we can set its sequence
    ;;This way when we make partners they have the correct seq
    (loop for k from 1 to 4 do
      (loop for i from 1 to 22 do
	(progn
	  ;;(break "scaff ~A" (scaffold ori))
	  (add-to-scaffold ori (scaffold-helix k i))
	  (when (evenp i)
	    ;; (let ((es (edge-staple ori k i
	    ;; 			   (nth (- (length scaff) 2) scaff)
	    ;; 			   (nth (- (length scaff) 1) scaff))))
	    ;;   (add-parent es ori)
	    ;;   (add-prop es :k k)
	    ;;   (add-prop es :i i)
	    ;;   (add-to-edge-staples ori es))    ;TODO: Edge staple logic later
	    ;; add scaffold-loops
	    (unless (and (= 4 k) (= 22 i))
	      (add-to-scaffold ori (SMALL::scaffold-loop k i)))
	    ))))    
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
    (setf (stap-bridges ori) (staple-bridges ori)
	  (int-staps ori) (internal-staples ori)
	  (u-staps ori) (u-staples ori))
    ori))





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



(defun tile-connection-staples (t1 k1 i1  t2 k2 i2 &key (overlap-len 4) (bridging-ss-len 1))
  (let* ((h1_k1_i1 (small::find-obj-with-props (SMALL::scaffold t1)
					       `((:i . ,i1) (:k . ,k1))))
	 (h1_k1_i1+1 (small::find-obj-with-props (SMALL::scaffold t1)
						 `((:i . ,(+ i1 1)) (:k . ,k1))))
	 (h2_k2_i2 (small::find-obj-with-props (SMALL::scaffold t2)
					       `((:i . ,i2) (:k . ,k2))))
	 (h2_k2_i2-1 (small::find-obj-with-props (SMALL::scaffold t2)
						 `((:i . ,(- i2 1)) (:k . ,k2))))
	 stap1 stap2)
    (multiple-value-bind (stap nts)
	(SMALL::create-staple `((:obj ,h2_k2_i2  :start 0 :end ,overlap-len  :from-3end nil)
				(:single-strand t :num-nts ,bridging-ss-len)
				(:obj ,h1_k1_i1  :start 0 :end 16  :from-3end t)
				(:obj ,h1_k1_i1+1  :start 0 :end 16  :from-3end nil)))
      (setf stap1 stap))
    (multiple-value-bind (stap nts)
	(SMALL::create-staple `((:obj ,h2_k2_i2-1  :start 0 :end 16  :from-3end t)
				(:obj ,h2_k2_i2  :start ,overlap-len :end 16  :from-3end nil)))
      (setf stap2 stap))
    ;(break "~A"  (list stap1 stap2))
    (list stap1 stap2)
    ))




(defun connect-tiles (t1 k1 t2 k2 &key (overlap-len 4) bridging-ss-len)
  "connects tile t1's side k1 to tile t2's side k2"
  (let* ((i1s '(1 5 9 13 17 21))
	 (i2s (mapcar #'(lambda (x)
			  (- (+ *2r* 1) x))
		      i1s)))
    (mapcar #'(lambda (i1 i2)
		(tile-connection-staples t1 k1 i1 t2 k2 i2
					 :overlap-len overlap-len
					 :bridging-ss-len bridging-ss-len))
	    i1s i2s)))
  



(defun u-staple (tile k1 i1 s1 e1 f3e1 k2 i2 s2 e2 f3e2 )
  "creates an s-shaped staple strand to hold tile helices i, i+1 together.
Starts are taken from tile edges"
  (let* ((h1 (SMALL::find-obj-with-props (scaffold tile)
					 `((:i . ,i1) (:k . ,k1))))
	 (h2 (small::find-obj-with-props (scaffold tile)
					 `((:i . ,i2) (:k . ,k2)))))
    (SMALL::create-staple `((:obj ,h1  :start ,s1 :end ,e1 :from-3end ,f3e1)
			    (:obj ,h2  :start ,s2 :end ,e2 :from-3end ,f3e2)))))

(defun u-staples (tile)
  (let* ((u1s ;; Staple u in row 1
	   (loop for k from 1 to 4 collect
				   (u-staple tile  k 2 18 25 t k 1 0 16 nil)))
	 (u4s ;; Staple u in row 1
	   (loop for k from 1 to 4 collect
				   (u-staple tile  k 4 13 29 t k 5 30 38 nil)))
	 (u9s ;; Staple u in row 1
	   (loop for k from 1 to 4 collect
				   (u-staple tile  k 10 27 35 t k 9 10 26 nil))))
    (list u1s u4s u9s)))
	 


(defun s-staple (tile k i starts lengths)
  "creates an s-shaped staple strand to hold tile helices i, i+1 and i+2 together.
Starts are taken from tile edges"
  (let* ((hi (SMALL::find-obj-with-props (scaffold tile)
					  `((:i . ,i) (:k . ,k))))
	 (hi+1 (small::find-obj-with-props (scaffold tile)
					  `((:i . ,(+ i 1)) (:k . ,k))))
	 (hi+2 (small::find-obj-with-props (scaffold tile)
					   `((:i . ,(+ i 2)) (:k . ,k))))
	 (ends (mapcar #'+ starts lengths)))
    (if (evenp i) ;;TODO: Add error checking on i and k
	(SMALL::create-staple `((:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end nil)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end t)
				(:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end nil)))
	(SMALL::create-staple `((:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end t)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end nil)
				(:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end t))))))
	 
			 
		       
	 
(defmethod internal-staples ((tile dna-tile))
  (let (staps)
    (setf staps
	  (loop for k from 1 to 4
		collect
		(loop
		  for i from 2 to 20 by 2
		  collect
		  (s-staple tile k i '(23 16 16) '(8 15 7)))))
    (setf staps (append staps
			(loop for k from 1 to 4
			      collect
			      (loop
				for i from 5 to 18 by 2
				collect
				(s-staple tile k i '(39 31 31) '(8 16 8))))))
    (setf staps (append staps
			(loop for k from 1 to 4
			      collect
			      (loop
				for i from 6 to 16 by 2
				collect
				(s-staple tile k i '(55 47 47) '(8 16 8))))))
    (setf staps (append staps
			(loop for k from 1 to 4
			      collect
			      (loop
				for i from 7 to 15 by 2
				collect
				(s-staple tile k i '(70 63 63) '(8 15 7))))))

    (setf staps (append staps
			(loop for k from 1 to 4
			      collect
			      (loop
				for i from 10 to 12 by 2
				collect
				(s-staple tile k i '(86 78 78) '(8 16 8))))))
    (setf staps (append staps
			(loop for k from 1 to 4
			      collect			    
				(s-staple tile k 11 '(102 94 94) '(8 16 8)))))))


  

  

(defun staple-bridges (tile)
  (let ((staps (remove nil
		       (loop for k from 1 to 4 collect
					       (remove nil
						       (loop for i from 1 to 22 collect
										(staple-bridge tile k i)))))))))




    

(defun staple-bridge (tile k i)
  (let* ((prevk (if (= k 1)
		    4
		    (- k 1)))
	 (nextk (if (= k 4)
		    1
		    (+ k 1))))
    (cond ((= i 2)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,(- *2r* i)) (:k . ,prevk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,(+ (- *2r* i) 1)) (:k . ,prevk))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h4 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,(+ i 1)) (:k . ,k))))
		  (stap (create-staple
			 `((:obj ,h1  :start 12 :end 20  :from-3end t)
			   (:obj ,h2  :start 0 :end 11  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h3  :start 0  :end 11 :from-3end t)
			   (:obj ,h4  :start 12  :end 20 :from-3end nil)))))
	     stap))
	  ((= i 3)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . 20) (:k . ,prevk))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 12  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 12  :from-3end t)))))
	     stap))
	  ((= i 4)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . 19) (:k . ,prevk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 21  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 13  :from-3end t)))))
	     stap))
	  ((= i 5)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,(+ i 1)) (:k . ,k))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . 18) (:k . ,prevk))))
		  (stap (create-staple
			 `((:obj ,h1  :start 22 :end 30  :from-3end t)
			   (:obj ,h2  :start 0 :end 22  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h3  :start 0 :end 14  :from-3end t)))))
	     stap))
	  ((= i 6)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,17) (:k . ,prevk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . 7) (:k . ,k))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 7  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 14  :from-3end t)
			   (:obj ,h3  :start 16 :end 23  :from-3end nil)))))
	     stap))
	  ((= i 7)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,8) (:k . ,k))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,7) (:k . ,k))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,16) (:k . ,prevk))))
		  (h4 (small::find-obj-with-props (scaffold tile)
						  `((:i . 15) (:k . ,prevk))))
		  (stap (create-staple
			 `((:obj ,h1  :start 9 :end 17 :from-3end t)
			   (:obj ,h2  :start 0 :end 8 :from-3end nil)
			   (:single-strand t)
			   (:obj ,h3  :start 0 :end 8  :from-3end t)
			   (:obj ,h4  :start 9 :end 17  :from-3end nil)))))
	     stap))
	  ((= i 8)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . 15) (:k . ,prevk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 9  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 9  :from-3end t)))))
	     stap))
	  ((= i 9)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . 14) (:k . ,prevk))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 10  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 18  :from-3end t)))))
	     stap))
	  ((= i 10)
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . 13) (:k . ,prevk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,(+ i 1)) (:k . ,k))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 11  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 19  :from-3end t)
			   (:obj ,h3  :start 19 :end 27  :from-3end nil)))))
	     stap))
	  ((and (= i 11) (or (= k 1) (= k 3)))
	   (let* ((h1 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,nextk))))
		  (h2 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,12) (:k . ,k))))
		  (h3 (small::find-obj-with-props (scaffold tile)
						  `((:i . ,i) (:k . ,k))))
		  (h4 (small::find-obj-with-props (scaffold tile)
						  `((:i . 12) (:k . ,prevk))))
		  (stap (create-staple
			 `((:obj ,h1  :start 0 :end 11 :from-3end nil)
			   (:single-strand t)
			   (:obj ,h2  :start 0 :end 11 :from-3end t)
			   (:obj ,h3  :start 0 :end 11  :from-3end nil)
			   (:single-strand t)
			   (:obj ,h4  :start 0 :end 11  :from-3end t)))))
	     stap))
	  
	  (t nil))))






    
(wmdna "ot1" (scaffold (make-instance 'dna-tile)))
