(in-package :small)


(defclass/std dna-tile (dna)
  ()
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

(defun make-dna-tile (tfms)
  "Creates an instance of a tile object"
  (make-instance 'dna-tile :tfms tfms))


(defmethod initialize-instance :after ((tile dna-tile) &key)
  "Creates DNA origamis scaffold subobjs (scaff helices, loops, bridges), joins them together to form them scaffold strand and updates their base seq. Then creates origamis staples and staple bridges. Saves these to the instance of the object"
  ;; (loop for k from 1 to 4 collect
  ;; 			  ((add-to-scaff (create-triangle tile k))
  ;; 			   (when (between k 1 3)
  ;; 			     (add-to-scaff (scaff-bridge k)))))
  ;; (create-staples)
  )


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
   (defclass/std dna-tile-triangle (dna)
     
