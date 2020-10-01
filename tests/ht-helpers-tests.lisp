(in-package :small-tests)

(define-test "+ht"
  (let ((ht (make-hash-table))
	(ans (list "DNA-NT 0" "DNA-NT 1" "DNA-NT 2"
		   "DNA-NT 3" "DNA-NT 4" "DNA-NT 5")))
    (loop for i from 0 to 5 do
      (+ht ht (make-dna-nt))
;;      (format t "~& ~A ~%" (alexandria:hash-table-keys ht))))

      (is equal ans (alexandria:hash-table-keys ht)))))

		       

