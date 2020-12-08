(in-package #:small)

(let* ((t1  (make-instance 'dna-triangle))
       (af (alexandria::flatten (all-to-write t1))))
  ;(all-to-write t1)
  (overlapping-ends? (cddr (all-to-write t1))))



(defun overlapping-ends? (all)
  (let* ((1st (first all))
	 (rest (cdr all))
	 (overlaps (mapcar #'(lambda (x)
			       (overlaps? 1st x))			   
			   rest)))
    (if (cdr rest)
	(append overlaps (overlapping-ends? (cdr rest)))
	overlaps)))
	


    

(defun overlaps? (dna1 dna2)
  (cond ((eq (partner (5nt dna1)) (partner (5nt dna2)))
	 (list (partner (5nt dna1))
	       (5nt dna1)
	       (5nt dna2)
	       dna1
	       dna2
	       "DNA1: 5nt DNA2: 5nt"))
	((eq (partner (5nt dna1)) (partner (3nt dna2)))
	 (list (partner (5nt dna1))
	       (5nt dna1)
	       (3nt dna2)
	       dna1
	       dna2
	       "DNA1: 5nt DNA2: 3nt"))
	((eq (partner (3nt dna1)) (partner (3nt dna2)))
	 (list (partner (3nt dna1))
	       (3nt dna1)
	       (3nt dna2)
	       dna1
	       dna2
	       "DNA1: 3nt DNA2: 3nt"))
	((eq (partner (3nt dna1)) (partner (5nt dna2)))
	 (list (partner (3nt dna1))
	       (3nt dna1)
	       (5nt dna2)
	       dna1
	       dna2
	       "DNA1: 3nt DNA2: 5nt"))
	(t nil)))

(let* ((t1  (make-instance 'dna-triangle))
       (af (alexandria::flatten (all-to-write t1)))
       (four (first (subseq af 4 5)))
       (one (first (subseq af 1 2))))
  (overlapping-ends? one four))





  
