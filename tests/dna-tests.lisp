(in-package :small-tests)



;(test "(dna-connect o1 o2)")
(define-test "(dna-connect o1 o2)"
  (let* ((s1 (make-dna-strand))
	 (s2 (make-dna-strand))
	 (s1p (prev s1))
	 (s2n (next s2)))
					;    (connect s1 s2 :new t)
        (connect s1 s2)
    (is eq (prev s2) s1)
    (is eq (next s1) s2)
    (is eq s1p (prev s1))
    (is eq s2n (next s2))))
