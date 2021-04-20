(in-package :small)

(defclass/std crossover ()
  ((nt1 :doc "The nt whichs partner will be on the 5' end of the crossover")
   (nt2 :doc "The nt whichs partner will be on the 3' end of the crossover")
   (pt1 :doc "5' of crossover")
   (pt2 :doc "3' of crossover")
   (dist :doc "The length in nm between pt1 and pt2")))


;; ==========================Helper functions =====================================
(defparameter *antiparallel-dotproduct* -1.0 "Variable that defines a number that the dot product between two vectors should be LESS THAN OR EQUAL TO to be considered antiparallel")

(defun antiparallelp (strand1 strand2)
  "Returns t if strands are antiparallel. Antiparallel means dot-product between vn <= *antiparallel-dotproduct*")
    

(defun all-potential-crossovers (strands &key (cutoff-dist 2.0))
  "strands is a list of DNA-HELIX-STRANDS
Returns a list of CROSSOVERs will have distances between them <= cutoff-dist"
  )
  

