(in-package #:small-tests)


(defun make-tile-scaffold ()
  )





(let* ((nts (alexandria:flatten
	     (loop
	       for k from 1 to 4
	       collect
	       (loop
		 for i from 1 to 22
		 collect
		 (loop
		   for j from 1 to (small::ai i)
		   collect
		   (if (oddp i)
		       (small::make-dna-nt :cm (small::scaffold-coords k i j :cm t)
					   :vbb (small:as-unit-vec
						 (magicl:.- (small::scaffold-coords k i j :cm nil)
							    (small::helix-axis-coords k i j)))
					   
					   :vn (small:as-unit-vec (if (oddp i)
								      (magicl:.- (small::helix-axis-coords k i 1)
										 (small::helix-axis-coords k i 2))
								      (magicl:.- (small::helix-axis-coords k i 2)
										 (small::helix-axis-coords k i 1)))))
		       (small::make-dna-nt :cm (small::scaffold-coords k i (- (small::ai i) j) :cm t)
					   :vbb (small:as-unit-vec
						 (magicl:.- (small::scaffold-coords k i (- (small::ai i) j) :cm nil)
							    (small::helix-axis-coords k i (- (small::ai i) j))))
					   :vn (small:as-unit-vec (if (oddp i)
								      (magicl:.- (small::helix-axis-coords k i 1)
										 (small::helix-axis-coords k i 2))
								      (magicl:.- (small::helix-axis-coords k i 2)
										 (small::helix-axis-coords k i 1)))))
		       ))))))
       (pts (reverse nts)))
  pts

  ;; (break "nts ~A ~%"  (subseq nts 0 10))
  ;;(break "pts ~A"  (subseq pts 0 10))
  (small::connect-nts nts)
  ;;(small::connect-nts pts)
  ;;(break "~A~%~A" (first pts) (last nts))
  (break "~A~%~A~%~A" (small::cm (first nts)) (small::vbb (first nts)) (small::vn (first nts)))
  (small::write-oxdna (first nts) :filename "scaff"))
;;(small::write-oxdna (first pts) :filename "stap"))

