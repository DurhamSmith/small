(in-package :small-tests)


(deftest "(grow dna-strand)"
  (let* ((s (make-dna-strand))
	 (nt1 (make-dna-nt))
	 (nt2 (make-dna-nt))
	 (nt3 (make-dna-nt))
	 (nt4 (make-dna-nt)))
    (is eq nil (nts s))
    (grow s (list nt1 nt2 nt3 nt4))
    (is eq nt1 (nth-nt s 0))
    (is eq nt2 (nth-nt s 1))
    (is eq nt3 (nth-nt s 2))
    (is eq nt4 (nth-nt s 3))
    (is eq nt1 (5end s))
    (is eq nt4 (3end s))
    (is eq (list nt1 nt2 nt3 nt4)
	(nts strand :all nil))))
	 
(deftest "(add-nt dna-strand)"
  (let* ((s (make-dna-strand))
	 (nt1 (make-dna-nt))
	 (nt2 (make-dna-nt))
	 (nt3 (make-dna-nt)))
    (is eq nil (5end))
    (is eq nil (3end))
    (add-nt s nt1)
    (is eq nt1 (5end s :nt t))
    (is eq nt1 (3end s :nt t))
    (is eq (list nt1) (nts s))
    (add-nt s nt2)
    (is eq nt1 (5end s :nt t))
    (is eq nt2 (3end s :nt t))
    (is eq (list nt1 nt2) (nts s))
    (add-nt s nt3 :5end t)
    (is eq nt3 (5end s :nt t))
    (is eq nt2 (3end s :nt t))
    (is eq (list nt3 nt1 nt2) (nts s)))
    ;;TODO more tests for multi-stranded cases
    )
    
    

    (add-nt s nt2)
    (is eql '(nt1 nt2) (nucleotides s :all nil))))

	
   
    
