(in-package :small)

(defparameter *m13mp18*
  "AATGCTACTACTATTAGTAGAATTGATGCCACCTTTTCAGCTCGCGCCCCAAATGAAAATATAGCTAAACAGGTTATTGACCATTTGCGAAATGTATCTAATGGTCAAACTAAATCTACTCGTTCGCAGAATTGGGAATCAACTGTTATATGGAATGAAACTTCCAGACACCGTACTTTAGTTGCATATTTAAAACATGTTGAGCTACAGCATTATATTCAGCAATTAAGCTCTAAGCCATCCGCAAAAATGACCTCTTATCAAAAGGAGCAATTAAAGGTACTCTCTAATCCTGACCTGTTGGAGTTTGCTTCCGGTCTGGTTCGCTTTGAAGCTCGAATTAAAACGCGATATTTGAAGTCTTTCGGGCTTCCTCTTAATCTTTTTGATGCAATCCGCTTTGCTTCTGACTATAATAGTCAGGGTAAAGACCTGATTTTTGATTTATGGTCATTCTCGTTTTCTGAACTGTTTAAAGCATTTGAGGGGGATTCAATGAATATTTATGACGATTCCGCAGTATTGGACGCTATCCAGTCTAAACATTTTACTATTACCCCCTCTGGCAAAACTTCTTTTGCAAAAGCCTCTCGCTATTTTGGTTTTTATCGTCGTCTGGTAAACGAGGGTTATGATAGTGTTGCTCTTACTATGCCTCGTAATTCCTTTTGGCGTTATGTATCTGCATTAGTTGAATGTGGTATTCCTAAATCTCAACTGATGAATCTTTCTACCTGTAATAATGTTGTTCCGTTAGTTCGTTTTATTAACGTAGATTTTTCTTCCCAACGTCCTGACTGGTATAATGAGCCAGTTCTTAAAATCGCATAAGGTAATTCACAATGATTAAAGTTGAAATTAAACCATCTCAAGCCCAATTTACTACTCGTTCTGGTGTTTCTCGTCAGGGCAAGCCTTATTCACTGAATGAGCAGCTTTGTTACGTTGATTTGGGTAATGAATATCCGGTTCTTGTCAAGATTACTCTTGATGAAGGTCAGCCAGCCTATGCGCCTGGTCTGTACACCGTTCATCTGTCCTCTTTCAAAGTTGGTCAGTTCGGTTCCCTTATGATTGACCGTCTGCGCCTCGTTCCGGCTAAGTAACATGGAGCAGGTCGCGGATTTCGACACAATTTATCAGGCGATGATACAAATCTCCGTTGTACTTTGTTTCGCGCTTGGTATAATCGCTGGGGGTCAAAGATGAGTGTTTTAGTGTATTCTTTTGCCTCTTTCGTTTTAGGTTGGTGCCTTCGTAGTGGCATTACGTATTTTACCCGTTTAATGGAAACTTCCTCATGAAAAAGTCTTTAGTCCTCAAAGCCTCTGTAGCCGTTGCTACCCTCGTTCCGATGCTGTCTTTCGCTGCTGAGGGTGACGATCCCGCAAAAGCGGCCTTTAACTCCCTGCAAGCCTCAGCGACCGAATATATCGGTTATGCGTGGGCGATGGTTGTTGTCATTGTCGGCGCAACTATCGGTATCAAGCTGTTTAAGAAATTCACCTCGAAAGCAAGCTGATAAACCGATACAATTAAAGGCTCCTTTTGGAGCCTTTTTTTTGGAGATTTTCAACGTGAAAAAATTATTATTCGCAATTCCTTTAGTTGTTCCTTTCTATTCTCACTCCGCTGAAACTGTTGAAAGTTGTTTAGCAAAATCCCATACAGAAAATTCATTTACTAACGTCTGGAAAGACGACAAAACTTTAGATCGTTACGCTAACTATGAGGGCTGTCTGTGGAATGCTACAGGCGTTGTAGTTTGTACTGGTGACGAAACTCAGTGTTACGGTACATGGGTTCCTATTGGGCTTGCTATCCCTGAAAATGAGGGTGGTGGCTCTGAGGGTGGCGGTTCTGAGGGTGGCGGTTCTGAGGGTGGCGGTACTAAACCTCCTGAGTACGGTGATACACCTATTCCGGGCTATACTTATATCAACCCTCTCGACGGCACTTATCCGCCTGGTACTGAGCAAAACCCCGCTAATCCTAATCCTTCTCTTGAGGAGTCTCAGCCTCTTAATACTTTCATGTTTCAGAATAATAGGTTCCGAAATAGGCAGGGGGCATTAACTGTTTATACGGGCACTGTTACTCAAGGCACTGACCCCGTTAAAACTTATTACCAGTACACTCCTGTATCATCAAAAGCCATGTATGACGCTTACTGGAACGGTAAATTCAGAGACTGCGCTTTCCATTCTGGCTTTAATGAGGATTTATTTGTTTGTGAATATCAAGGCCAATCGTCTGACCTGCCTCAACCTCCTGTCAATGCTGGCGGCGGCTCTGGTGGTGGTTCTGGTGGCGGCTCTGAGGGTGGTGGCTCTGAGGGTGGCGGTTCTGAGGGTGGCGGCTCTGAGGGAGGCGGTTCCGGTGGTGGCTCTGGTTCCGGTGATTTTGATTATGAAAAGATGGCAAACGCTAATAAGGGGGCTATGACCGAAAATGCCGATGAAAACGCGCTACAGTCTGACGCTAAAGGCAAACTTGATTCTGTCGCTACTGATTACGGTGCTGCTATCGATGGTTTCATTGGTGACGTTTCCGGCCTTGCTAATGGTAATGGTGCTACTGGTGATTTTGCTGGCTCTAATTCCCAAATGGCTCAAGTCGGTGACGGTGATAATTCACCTTTAATGAATAATTTCCGTCAATATTTACCTTCCCTCCCTCAATCGGTTGAATGTCGCCCTTTTGTCTTTGGCGCTGGTAAACCATATGAATTTTCTATTGATTGTGACAAAATAAACTTATTCCGTGGTGTCTTTGCGTTTCTTTTATATGTTGCCACCTTTATGTATGTATTTTCTACGTTTGCTAACATACTGCGTAATAAGGAGTCTTAATCATGCCAGTTCTTTTGGGTATTCCGTTATTATTGCGTTTCCTCGGTTTCCTTCTGGTAACTTTGTTCGGCTATCTGCTTACTTTTCTTAAAAAGGGCTTCGGTAAGATAGCTATTGCTATTTCATTGTTTCTTGCTCTTATTATTGGGCTTAACTCAATTCTTGTGGGTTATCTCTCTGATATTAGCGCTCAATTACCCTCTGACTTTGTTCAGGGTGTTCAGTTAATTCTCCCGTCTAATGCGCTTCCCTGTTTTTATGTTATTCTCTCTGTAAAGGCTGCTATTTTCATTTTTGACGTTAAACAAAAAATCGTTTCTTATTTGGATTGGGATAAATAATATGGCTGTTTATTTTGTAACTGGCAAATTAGGCTCTGGAAAGACGCTCGTTAGCGTTGGTAAGATTCAGGATAAAATTGTAGCTGGGTGCAAAATAGCAACTAATCTTGATTTAAGGCTTCAAAACCTCCCGCAAGTCGGGAGGTTCGCTAAAACGCCTCGCGTTCTTAGAATACCGGATAAGCCTTCTATATCTGATTTGCTTGCTATTGGGCGCGGTAATGATTCCTACGATGAAAATAAAAACGGCTTGCTTGTTCTCGATGAGTGCGGTACTTGGTTTAATACCCGTTCTTGGAATGATAAGGAAAGACAGCCGATTATTGATTGGTTTCTACATGCTCGTAAATTAGGATGGGATATTATTTTTCTTGTTCAGGACTTATCTATTGTTGATAAACAGGCGCGTTCTGCATTAGCTGAACATGTTGTTTATTGTCGTCGTCTGGACAGAATTACTTTACCTTTTGTCGGTACTTTATATTCTCTTATTACTGGCTCGAAAATGCCTCTGCCTAAATTACATGTTGGCGTTGTTAAATATGGCGATTCTCAATTAAGCCCTACTGTTGAGCGTTGGCTTTATACTGGTAAGAATTTGTATAACGCATATGATACTAAACAGGCTTTTTCTAGTAATTATGATTCCGGTGTTTATTCTTATTTAACGCCTTATTTATCACACGGTCGGTATTTCAAACCATTAAATTTAGGTCAGAAGATGAAATTAACTAAAATATATTTGAAAAAGTTTTCTCGCGTTCTTTGTCTTGCGATTGGATTTGCATCAGCATTTACATATAGTTATATAACCCAACCTAAGCCGGAGGTTAAAAAGGTAGTCTCTCAGACCTATGATTTTGATAAATTCACTATTGACTCTTCTCAGCGTCTTAATCTAAGCTATCGCTATGTTTTCAAGGATTCTAAGGGAAAATTAATTAATAGCGACGATTTACAGAAGCAAGGTTATTCACTCACATATATTGATTTATGTACTGTTTCCATTAAAAAAGGTAATTCAAATGAAATTGTTAAATGTAATTAATTTTGTTTTCTTGATGTTTGTTTCATCATCTTCTTTTGCTCAGGTAATTGAAATGAATAATTCGCCTCTGCGCGATTTTGTAACTTGGTATTCAAAGCAATCAGGCGAATCCGTTATTGTTTCTCCCGATGTAAAAGGTACTGTTACTGTATATTCATCTGACGTTAAACCTGAAAATCTACGCAATTTCTTTATTTCTGTTTTACGTGCAAATAATTTTGATATGGTAGGTTCTAACCCTTCCATTATTCAGAAGTATAATCCAAACAATCAGGATTATATTGATGAATTGCCATCATCTGATAATCAGGAATATGATGATAATTCCGCTCCTTCTGGTGGTTTCTTTGTTCCGCAAAATGATAATGTTACTCAAACTTTTAAAATTAATAACGTTCGGGCAAAGGATTTAATACGAGTTGTCGAATTGTTTGTAAAGTCTAATACTTCTAAATCCTCAAATGTATTATCTATTGACGGCTCTAATCTATTAGTTGTTAGTGCTCCTAAAGATATTTTAGATAACCTTCCTCAATTCCTTTCAACTGTTGATTTGCCAACTGACCAGATATTGATTGAGGGTTTGATATTTGAGGTTCAGCAAGGTGATGCTTTAGATTTTTCATTTGCTGCTGGCTCTCAGCGTGGCACTGTTGCAGGCGGTGTTAATACTGACCGCCTCACCTCTGTTTTATCTTCTGCTGGTGGTTCGTTCGGTATTTTTAATGGCGATGTTTTAGGGCTATCAGTTCGCGCATTAAAGACTAATAGCCATTCAAAAATATTGTCTGTGCCACGTATTCTTACGCTTTCAGGTCAGAAGGGTTCTATCTCTGTTGGCCAGAATGTCCCTTTTATTACTGGTCGTGTGACTGGTGAATCTGCCAATGTAAATAATCCATTTCAGACGATTGAGCGTCAAAATGTAGGTATTTCCATGAGCGTTTTTCCTGTTGCAATGGCTGGCGGTAATATTGTTCTGGATATTACCAGCAAGGCCGATAGTTTGAGTTCTTCTACTCAGGCAAGTGATGTTATTACTAATCAAAGAAGTATTGCTACAACGGTTAATTTGCGTGATGGACAGACTCTTTTACTCGGTGGCCTCACTGATTATAAAAACACTTCTCAGGATTCTGGCGTACCGTTCCTGTCTAAAATCCCTTTAATCGGCCTCCTGTTTAGCTCCCGCTCTGATTCTAACGAGGAAAGCACGTTATACGTGCTCGTCAAAGCAACCATAGTACGCGCCCTGTAGCGGCGCATTAAGCGCGGCGGGTGTGGTGGTTACGCGCAGCGTGACCGCTACACTTGCCAGCGCCCTAGCGCCCGCTCCTTTCGCTTTCTTCCCTTCCTTTCTCGCCACGTTCGCCGGCTTTCCCCGTCAAGCTCTAAATCGGGGGCTCCCTTTAGGGTTCCGATTTAGTGCTTTACGGCACCTCGACCCCAAAAAACTTGATTTGGGTGATGGTTCACGTAGTGGGCCATCGCCCTGATAGACGGTTTTTCGCCCTTTGACGTTGGAGTCCACGTTCTTTAATAGTGGACTCTTGTTCCAAACTGGAACAACACTCAACCCTATCTCGGGCTATTCTTTTGATTTATAAGGGATTTTGCCGATTTCGGAACCACCATCAAACAGGATTTTCGCCTGCTGGGGCAAACCAGCGTGGACCGCTTGCTGCAACTCTCTCAGGGCCAGGCGGTGAAGGGCAATCAGCTGTTGCCCGTCTCACTGGTGAAAAGAAAAACCACCCTGGCGCCCAATACGCAAACCGCCTCTCCCCGCGCGTTGGCCGATTCATTAATGCAGCTGGCACGACAGGTTTCCCGACTGGAAAGCGGGCAGTGAGCGCAACGCAATTAATGTGAGTTAGCTCACTCATTAGGCACCCCAGGCTTTACACTTTATGCTTCCGGCTCGTATGTTGTGTGGAATTGTGAGCGGATAACAATTTCACACAGGAAACAGCTATGACCATGATTACGAATTCGAGCTCGGTACCCGGGGATCCTCTAGAGTCGACCTGCAGGCATGCAAGCTTGGCACTGGCCGTCGTTTTACAACGTCGTGACTGGGAAAACCCTGGCGTTACCCAACTTAATCGCCTTGCAGCACATCCCCCTTTCGCCAGCTGGCGTAATAGCGAAGAGGCCCGCACCGATCGCCCTTCCCAACAGTTGCGCAGCCTGAATGGCGAATGGCGCTTTGCCTGGTTTCCGGCACCAGAAGCGGTGCCGGAAAGCTGGCTGGAGTGCGATCTTCCTGAGGCCGATACTGTCGTCGTCCCCTCAAACTGGCAGATGCACGGTTACGATGCGCCCATCTACACCAACGTGACCTATCCCATTACGGTCAATCCGCCGTTTGTTCCCACGGAGAATCCGACGGGTTGTTACTCGCTCACATTTAATGTTGATGAAAGCTGGCTACAGGAAGGCCAGACGCGAATTATTTTTGATGGCGTTCCTATTGGTTAAAAAATGAGCTGATTTAACAAAAATTTAATGCGAATTTTAACAAAATATTAACGTTTACAATTTAAATATTTGCTTATACAATCTTCCTGTTTTTGGGGCTTTTCTGATTATCAACCGGGGTACATATGATTGACATGCTAGTTTTACGATTACCGTTCATCGATTCTCTTGTTTGCTCCAGACTCTCAGGCAATGACCTGATAGCCTTTGTAGATCTCTCAAAAATAGCTACCCTCTCCGGCATTAATTTATCAGCTAGAACGGTTGAATATCATATTGATGGTGATTTGACTGTCTCCGGCCTTTCTCACCCTTTTGAATCTTTACCTACACATTACTCAGGCATTGCATTTAAAATATATGAGGGTTCTAAAAATTTTTATCCTTGCGTTGAAATAAAGGCTTCTCCCGCAAAAGTATTACAGGGTCATAATGTTTTTGGTACAACCGATTTAGCTTTATGCTCTGAGGCTTTATTGCTTAATTTTGCTAATTCTTTGCCTTGCCTGTATGATTTATTGGATGTT")

(defclass/std dna-tile (dna-origami)
  ()
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

(defun deg->rad (deg)
  (* (/ pi 180) deg))



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
;    (break "t ~A   c ~A   Cy ~A" theta coords cart-cyl)
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
	 (vbb0 (as-unit-vec (.- cm 5axis))))
 ;   (break "CM: ~A" cm)
    (helix-strand 5axis vn vbb0 (ai i))))

(defun scaffold-loop (k i)
  (let* ((c1 (scaffold-coords k i (ai i)))
	 (c2 (if (= i (* 2 *r*))
		 (scaffold-coords (+ k 1) 1 (ai 1))
		 (scaffold-coords k (+ i 1) (ai (+ i 1))))))
    (bridging-single-strand c1 c2 (v3 0 1 0))))


(make-dna-tile)
(defmethod initialize-instance :after ((ori dna-tile) &key)
  "Create the dna-origami chem-objs that represent the scaffold strand (scaff helixes = 2r = 22, scaff loops = 21, scaff bridges not included) and staple strands (TODO NUMBER, this excludes staples that also form staple bridges)"
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (progn
	(add-to-scaffold ori (scaffold-helix k i))
	(when (evenp i) ;; add scaffold-loops
	  (unless (and (= 4 k) (= 22 i))
	    (add-to-scaffold ori (SMALL::scaffold-loop k i)))))))

;;  (break "~& M13 Len:~A   ScaffLen:~A~%" (length *m13mp18*) (length (connected-nts (5nt (first (scaffold ori))))))
  (mapcar #'(lambda (nt base)
	      (setf (base nt) base))
	  (connected-nts (5nt (first (scaffold ori))))
	  (map 'list #'list *m13mp18*))
  (small::write-oxdna (5nt (first (scaffold ori))) :filename "full-tile")
  )



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

