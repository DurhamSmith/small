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
(defmethod create-triangle ((tile dna-tile) k)
  "Returns a triangle DNA structure correctly rotated for the position of its index, k"
  ;(make-tile :tfms equal)
)

(defmethod scaff-bridge ((tile dna-tile) k &key num-nts)
  "Create and returns a dna-single-strand which originates at nucleotide ai of triangle k's 2r-th helixes scaffold strand (SC_{k,2r,a_{2r}}) and ends at (SC_{(k+1),1,a_1}). num-nts nuclotides are added, if num-nts are not specified then $\frac{euclidean dist}{single-nt-len}"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dna-tile-triangle class. A composite chem-obj used in the construction of dna-tile 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/std dna-tile-triangle (dna-origami)
  (:documentation "A dna-origami chem-obj that contains the scaffold strand (including scaff loops, excluding scaff bridges) and staple strands (excluding those that that also form staple bridges)"))

(defun make-dna-tile-triangle (&key tfms)
  (make-instance 'dna-tile-triangle :tfms tfms))

(defmethod initialize-instance :after ((tri dna-tile-triangle) &key)
  "Create the dna-origami chem-objs that represent the scaffold strand (scaff helixes = 2r = 22, scaff loops = 21, scaff bridges not included) and staple strands (TODO NUMBER, this excludes staples that also form staple bridges)"
  ;; 1: Create scaff helixes
  ;;; Get starting positions of the bases on the 5' and 3' end of helix i
  ;;; Get vec for 5-3 dir of helix axis
  ;;; Get a unit vec that point in the direction from where the scaffold base joins the backbone (i.e opposite of oxdna backbone-base vec but the same as the paper theta on page 8 of the SI) in the two-dimensional plane of the j th base pair in the i th row). This vec should be perpendicular to v5-3 but doesn't need to be (e.g. in strained models)
  ;;; Create a double helix strand and add it to scaff-subobjs
  ;; 2: Create scaff loops
  ;;; vaxis points from helix i's last scaff nt (3' end) and helix i+1's first scaff nt (5' end)
  ;;; vbb is perp to vaxis and points 'kind of' (dotproduct is +) in helix i's axis 5'->3' direction
  ;;; nts are calculated based on single nt len and distance needed to be spanned
  ;; 3: Create staple stands
  ;;; (staple tri ((:start :end) (:connect-by :strand) (:start :end) (:connect-by :strand) (:start :end)))
  )

     
