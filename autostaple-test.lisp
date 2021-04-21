(in-package :small)




;; ==========================Helper functions =====================================
(defparameter *antiparallel-angle* pi "Variable that defines the minimum angle in radians for two helices to be considered antiparallel")

(defparameter *antiparallel-dotproduct* -0.99 "Variable that defines a number that the dot product between two vectors should be LESS THAN OR EQUAL TO to be considered antiparallel")

(defparameter *cutoff-dist* 2.0 "Distance in nm that nts must be within to be considered as a crossover point")

(defun antiparallelp (strand1 strand2)
  "Returns t if strands are antiparallel. Antiparallel means dot-product between the first nts vn <= *antiparallel-dotproduct*"
  (<= (dotproduct (vn (5nt strand1))
	     (vn (5nt strand2))) 
      *antiparallel-dotproduct*))

(defun antiparallel-strands (strand others)
  "strand: DNA-HELIX-STRAND
other: (list DNA-HELIX-STRAND)
Returns a list of all DNA strands that are antiparallel by antiparallelp
"
  (remove nil
	  (mapcar #'(lambda (x)
		      (when (antiparallelp strand x)
			x))
		  others)))

(defun potential-crossover-partners (strands)
  (when (cdr strands)
    (cons (list (car strands)
		(antiparallel-strands (car strands)
				      (cdr strands)))
	  (potential-crossover-partners (cdr strands)))))	   
	    
(dolist (x (potential-crossover-partners s) )
  (format t "~& ~A ~%" (length (second x))))      
    


(defun staple-crossovers (strand strands)
  "Returns all the crossovers between helices TODO as what?"
  (mapcar #'(lambda (s)
	      ) 
)
  

	      
(defun all-potential-crossovers (strands &key (cutoff-dist *cutoff-dist*))
  "strands is a list of DNA-HELIX-STRANDS
Returns a list of CROSSOVERs will have distances between them <= cutoff-dist"
  ;; 1: Recurse through strands to get all potential antiparallel partners (list (list hel-in-1-dir all-antiparallel-helices))
  ;; 2: double mapcar (over cdrs for second) for each car of potential antiparallel and create all possible crossovers
  ;; 3: remove crossovers if > cutoff-dist
  (let* ((ppts (potential-crossover-partners strands))
	 (crossovers (mapcar #'(lambda (pt)
				 (
				 ) 

	 
  )


		     


(defun within-x (x crossover nts)
  "Returns t if crossover is within x nts from any nt in nts")

(defun filter-crossovers (nts crossovers &key pts)
  "nts: (list DNA-NT)
crossovers: (list CROSSOVER)
Take a list of crossovers and nts and returns a new list without any crossover that have a nt in nts as one of their nt slot. if :pts=t only if the nt in nts are in the CROSSOVERs pt1/2 slot are they removed")
  

(defun make-staples (scaffold-nts crossovers)
  ;; 1: Get all applicable crossovers
  ;; 2: traverse by atleast min-helix-length then choose next pt-pair branch  if we cross a disallowed nt, stop strand creation, return whole strand if >= min-length, else error
;; keep track of crossed pt-pairs
; 3: if (intersection crossed-pt-pairs all-pt-pairs) is the empty set continue else repeat the process for next nt from 3' end of the scaffold that doesnt have a partner and isnt disallowed

  )
  

  
	    

;;;; ==========================Scratch Area========================
;; Testing antiparallel
(setf l (make-instance 'dna-square-lattice))
(setf s (scaffold l))
(length s)
(length (antiparallel-strands (car s) (cdr s)))
(wmdna "antiparallel" (first s) (antiparallel-strands (car (scaffold l)) (cdr (scaffold l))))


;;; Testing crossover creation
(make-crossovers (first s) (second s))
