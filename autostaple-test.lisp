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

;; ==============================================================================

(defun get-antiparallel-strands (strand others)
  "strand: DNA-HELIX-STRAND
others: (list DNA-HELIX-STRAND)
Returns a list of all DNA strands in others that are antiparallel to strand by antiparallelp
"
  (remove nil
	  (mapcar #'(lambda (x)
		      (when (antiparallelp strand x)
			x))
		  others)))

(defun unique-antiparallel-partners (strands)
  "Recursively looks through strands to find and group all antiparallel strands
Returns: (list DNA-HELIX-STRAND (list antiparallel-parter[DNA-HELIX-STRAND] ...))n
Importantly nth strand in strands partner strands will not include DNA-HELIX-STRAND [0,(n-1)] in strands. This allows us not to generate double crossovers when traversing this list"
  (when (cdr strands)
    (cons (list (car strands)
		(get-antiparallel-strands (car strands)
				      (cdr strands)))
	  (unique-antiparallel-partners (cdr strands)))))





;; (defun staple-crossovers (strand strands)
;;   "Returns all the crossovers between helices TODO as what?"
;;   (mapcar #'(lambda (s)
;;	      )
;; )



;; (defun all-potential-crossovers (strands &key (cutoff-dist *cutoff-dist*))
;;   "strands is a list of DNA-HELIX-STRANDS
;; Returns a list of CROSSOVERs will have distances between them <= cutoff-dist"


;;   ;; 3: remove crossovers if > cutoff-dist
;;     ;; 1: Recurse through strands to get all potential antiparallel partners (list (list hel-in-1-dir all-antiparallel-helices))
;;   (let* ((antiparallel-groups (unique-antiparallel-partners strands))
;;	 ;; 2: double mapcar (over cdrs for second) for each car of potential antiparallel and create all possible crossovers
;;	 (crossovers (mapcar #'(lambda (antiparallel-group)
;;				 ;; First entry of the nested list returned by unique-antiparallel-partners is the strand and the second is a nested list of partner strands"
;;				 (mapcar #'(lambda (partner-strand)
;;					     (strand-crossovers (car antiparallel-group)
;;							      partner-strand
;;							      :cutoff-dist cutoff-dist)
;; ;;					 (break partner-strand)

;;					     )
;;					 (second antiparallel-group)))
;;			     antiparallel-groups)))
;;     crossovers))




(defun all-potential-crossovers (strands &key (cutoff-dist *cutoff-dist*))
  "strands is a list of DNA-HELIX-STRANDS
Returns : (list (list CROSSOVER [1,n]) will have distances between them <= cutoff-dist. If one nt has more potential crossovers the returned as a list includes all af them"


  ;; 3: remove crossovers if > cutoff-dist
    ;; 1: Recurse through strands to get all potential antiparallel partners (list (list hel-in-1-dir all-antiparallel-helices))
  (let* ((antiparallel-groups (unique-antiparallel-partners strands))
	 ;; 2: double mapcar (over cdrs for second) for each car of potential antiparallel and create all possible crossovers
	 (crossovers (mapcar #'(lambda (antiparallel-group)
				 ;; First entry of the nested list returned by unique-antiparallel-partners is the strand and the second is a nested list of partner strands"
				 (make-crossovers (first antiparallel-group)
						  (second antiparallel-group)))
			     antiparallel-groups)))
    (reduce #'append crossovers)))

pc



(all-potential-crossovers s)


(mapcar #'find-best-crossover
	(reduce #'append pc))


(length
 (mapcar #'find-best-crossover
	  (reduce #'append pc)))


(length (resolve-conflicting-crossovers
 (mapcar #'find-best-crossover
	  (reduce #'append pc))
 (all-conflicting-crossovers
  (mapcar #'find-best-crossover
	  (reduce #'append pc)))))

(wmdna "all-crossovers" s
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c))))
	       (resolve-conflicting-crossovers
		(mapcar #'find-best-crossover
			(reduce #'append pc))
		(all-conflicting-crossovers
		 (mapcar #'find-best-crossover
			 (reduce #'append pc))))))



(resolve-conflicting-crossovers
 (mapcar #'find-best-crossover
	  (reduce #'append pc))
 (all-conflicting-crossovers
  (mapcar #'find-best-crossover
	  (reduce #'append pc))))



(unique-antiparallel-partners s)



(reduce #'append (remove-all-nils pc))

(flatten (remove-all-nils pc))

(remove-all-nils
 (map nil #'(lambda (pt)
		(strand-crossovers (first (first (unique-antiparallel-partners s)))
			     pt))
      (second (first (unique-antiparallel-partners s)))))


(make-crossovers (first (first (unique-antiparallel-partners s)))
		 (second (first (unique-antiparallel-partners s))))

(reduce #'append
	(mapcar  #'(lambda (pt)
		(strand-crossovers (first (first (unique-antiparallel-partners s)))
			     pt))
     (second (first (unique-antiparallel-partners s)))))
(append '(1) '(2) nil '(3))


(mapcar #'(lambda (partner-strand)
	    (strand-crossovers (first (first (unique-antiparallel-partners s)))
			     partner-strand
			     :cutoff-dist *cutoff-dist*))
	(second (first (unique-antiparallel-partners s))))

 (mapcar #'(lambda ()
	     )
 (second (unique-antiparallel-partners s)))
 )

(second (first (unique-antiparallel-partners s)))

(all-conflicting (remove-all-nils pc))



;;;; ==========================Scratch Area========================
;; Testing antiparallel
;; 1
(progn
  (setf l (make-instance 'dna-square-lattice)
	s (scaffold l)
	cross (remove-nilr (strand-crossovers (first s) (second s)))
	fc (flatten cross)
	bc (mapcar #'find-best-crossover
		   (remove-nilr cross))
	pc (all-potential-crossovers s)))

(remove-all-nils pc)

(length s)
(length (antiparallel-strands (car s) (cdr s)))
(wmdna "antiparallel" (first s) (get-antiparallel-strands (car (scaffold l)) (cdr (scaffold l))))

;; 2
(dolist (x (unique-antiparallel-partners s) )
  (format t "~& ~A ~%" (length (second x))))

;;; Testing crossover creation


(1Dp (planarp (caar (remove-nilr (strand-crossovers
		    (first s)
		    (second s)
		    :cutoff-dist 2)))))

(mapcar #'(lambda (x)
	    (planarp x)
	    )
	(flatten (remove-nilr (strand-crossovers (first s) (second s)))))

(mapcar #'(lambda (x)
	    (1Dp (planarp x))
	    )
	(flatten (remove-nilr (strand-crossovers (first s) (second s)))))

(remove-nilr (strand-crossovers
	      (first s)
	      (second s)
	      :cutoff-dist 2))

(mapcar #'(lambda (x)
	    (bbdot x)
	    )
	(flatten (remove-nilr (strand-crossovers (first s) (second s)))))

(mapcar #'(lambda (x)
	    (rad->deg (bbangle x))
	    )
	(flatten (remove-nilr (strand-crossovers
			       (first s)
			       (second s)
			       :cutoff-dist 2))))

(remove-nilr (strand-crossovers (first s) (second s)))
(wmdna "crossovers" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
	       (alexandria:flatten (remove-nilr (strand-crossovers
						 (first s)
						 (second s)
						 :cutoff-dist 1.9)))))



(wmdna "crossovers-best" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
	       (mapcar #'find-best-crossover
		       (remove-nilr (strand-crossovers (first s) (second s))))))


;; Check best crossover
(mapcar #'find-best-crossover
	(remove-nilr (strand-crossovers (first s) (second s))))

;; check conflicting
(conflictingp (third (mapcar #'find-best-crossover
			     (remove-nilr (strand-crossovers (first s) (second s)))))
	      (second (mapcar #'find-best-crossover
		  (remove-nilr (strand-crossovers (first s) (second s))))))

(conflictingp (first (mapcar #'find-best-crossover
			     (remove-nilr (strand-crossovers (first s) (second s)))))
	      (second (mapcar #'find-best-crossover
		  (remove-nilr (strand-crossovers (first s) (second s))))))


(set-difference bc (flatten (mapcar #'worst-crossovers (all-conflicting-crossovers bc))))
(flatten (mapcar #'worst-crossovers (all-conflicting-crossovers bc)))
(flatten (mapcar #'find-best-crossover (all-conflicting-crossovers bc)))

(intersection bc (flatten (mapcar #'worst-crossovers (all-conflicting-crossovers bc)))


(remove-nilr (all-conflicting-crossovers bc))

(find-best-crossover (second cross))
(worst-crossovers (second cross))

(length fc)

;; We run into issues with double crossover generation both when the scaffold strand has two potential targets (here we select the best) or when the scaffold strand has a potetial target DNA-NT for the CORRECT staple that +1 or -1 from the correct target DNA-NT on the scaffold strand.



(wmdna "conflict-1" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
	       (list (first (second cross)))))


(wmdna "conflict-solved" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
	       (set-difference bc (flatten (mapcar #'worst-crossovers (all-conflicting-crossovers bc))))))

(wmdna "conflicts-worst" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
	       (intersection bc
			     (flatten
			      (mapcar #'worst-crossovers (all-conflicting-crossovers bc))))))

(find-best-crossover (second cross))
(length (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c)))
		   )
		(alexandria:flatten (remove-nilr (strand-crossovers (first s) (second s) :cutoff-dist 1.9)))))


(mapcar #'find-best-crossovernnn
	 (remove-nilr (strand-crossovers (first s) (second s))))

(length (remove-nilr (strand-crossovers (first s) (second s))))

(wmdna "conflicts-resolved" (first s) (second s)
       (mapcar #'(lambda (c)
		   (list (make-partner (nt1 c))
			 (make-partner (nt2 c))))
	       (resolve-conflicting-crossovers
			      cross
			      (all-conflicting-crossovers bc))))


;;=========================== Unimplemented ===========================================================
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
n
  )
