(in-package #:small-tests)

(define-test dna-nt-suite)  
  
(define-test "Test make-dna-nt creates a DNA-NT CHEM-OBJ"
  :parent dna-nt-suite
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (of-type 'SMALL::DNA-NT nt)
    (is #'magicl:= (small::cm nt) cm)
    (setf cm nil)
    (of-type 'MAGICL::VECTOR/DOUBLE-FLOAT (small::cm nt)) 
    )
  )



(define-test "Test oxdna-config"
  :parent dna-nt-suite
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (is equal
	"1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 0.0 0.0"
	(oxdna-config nt))))

(define-test "Test DNA-NT:CONNECT"
  :parent dna-nt-suite
  (let* ((v (v3 1 1 1))
	 (-v (magicl:scale v -1))
	 (nt1 (make-dna-nt :cm v :vbb v :vn v))
	 (nt2 (make-dna-nt :cm -v :vbb -v :vn -v)))
    (isnt eq nt1 nt2)
    (connect nt1 nt2)
    (is eq nt2 (next nt1))))

	 
    

