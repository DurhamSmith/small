(in-package #:small-tests)


(defun make-tile-scaffold ()
  )





(let* ((nts (alexandria:flatten (loop for k from 1 to 4 collect
							(loop for i from 1 to 22 collect
										 (loop for j from
										       1 to (small::ai i) collect
													  (small::make-dna-nt :cm (small::scaffold-coords k i j :cm t)
														       :vbb (small::theta-1ij-staple i j)
														       :vn (small:as-unit-vec (if (oddp i)
																	    (magicl:.- (small::helix-axis-coords k i 1)
																		       (small::helix-axis-coords k i 2))
																	    (magicl:.- (small::helix-axis-coords k i 2)
																		       (small::helix-axis-coords k i 1))))))))))
       (pts (reverse nts)))
  pts
  (small::connect-nts nts)
  (small::connect-nts pts)
  (small::write-oxdna (first nts) :filename "scaff")
  (small::write-oxdna (first pts) :filename "stap"))
  
