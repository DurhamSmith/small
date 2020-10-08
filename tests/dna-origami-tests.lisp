(in-package #:small-tests)



(define-test "add-to-scaffold ((ori dna-origami) (scaff-obj dna))"
  (let* ((ori (make-instance 'SMALL::dna-origami))
	 (nt1 (make-dna-nt))
	 (nt2 (make-dna-nt)))
    (with-accessors ((scaffold SMALL::scaffold)) ori
					;    (break "~A" scaffold)
      (false scaffold)
      ;; (skip "NO"
      ;; (SMALL::add-to-scaffold ori nt1)
      ;; (is = 1 (length scaffold))
      ;; (is eq (first scaffold) nt1)
      ;; (SMALL::add-to-scaffold ori nt2)
      ;; (is = 2 (length scaffold))
      ;; (is eq (first scaffold) nt1)
      ;; (is eq (second scaffold) nt2)
      ;; (is equal (list nt1 nt2) (connected-nts nt2)))
      )))

(test "add-to-scaffold ((ori dna-origami) (scaff-obj dna))" :report 'interactive)
       
       

(small::scaffold (make-instance 'SMALL::dna-origami))


