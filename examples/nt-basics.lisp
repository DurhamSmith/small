(let* ((cm1 (v3 0.600 0 0))
       (cm2 (v3 0.495 0.340 0.340))
       (cm3 (v3 0.215 0.560 0.680))
       (cm4 (v3 -0.140 0.584 1.020))
       (vbb1 (v3 1.000  0.000 0.000))
       (vbb2 (v3 0.824  0.566 0.000))
       (vbb3 (v3 0.359  0.933 0.000))
       (vbb4 (v3 -0.233 0.973 0.000))
       (vn (v3 0 0 1))
       (nt1 (make-instance 'dna-nt
			   :base "A"
			   :cm cm1
			   :vn vn
			   :vbb vbb1))
       (nt2 (make-instance 'dna-nt
			   :base "A"
			   :cm cm2
			   :vn vn
			   :vbb vbb2))
       (nt3 (make-instance 'dna-nt
			   :base "A"
			   :cm cm3
			   :vn vn
			   :vbb vbb3))
       (nt4 (make-instance 'dna-nt
			   :base "A"
			   :cm cm4
			   :vn vn
			   :vbb vbb4)))
  (wmdna "unconnected" nt1 nt2 nt3 nt4)
  (connect-nts nt1 nt2 nt3 nt4)
  (wmdna "connected" nt1 ))
       
