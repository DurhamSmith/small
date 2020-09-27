(in-package #:small-tests)

(define-test dna-nt-suite)  
  
(define-test "Test make-dna-nt creates a DNA-NT CHEM-OBJ"
  :parent dna-nt-suite
  (let* ((vbb (v3 1 0 0))
	 (nt 1))
    (of-type 'SMALL::DNA-NT (make-dna-nt))))


