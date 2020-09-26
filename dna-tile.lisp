(in-package :small)


(defclass/std dna-tile (dna)
  ()
  (:documentation "An implementation the DNA tile of Tikhomirov et al https://www.nature.com/articles/nnano.2016.256"))

(defun make-dna-tile (tfms)
  "Creates an instance of a tile object"
  (make-instance 'dna-tile :tfms tfms))


(defmethod initialize-instance :after ((tile tile) &key)
  "Creates DNA origamis scaffold subobjs (scaff helices, loops, bridges), joins them together to form them scaffold strand and updates their base seq. Then creates origamis staples and staple bridges. Saves these to the instance of the object"
  (loop for k from 1 to 4 collect
			  ((add-to-scaff (create-triangle tile k))
			   (when (between k 1 3)
			     (add-to-scaff (scaff-bridge k)))))
  (create-staples)
  
  
			   
  t)

