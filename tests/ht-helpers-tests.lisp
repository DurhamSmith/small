(in-package :small-tests)

(define-test "+ht"
  (let ((ht (make-hash-table))
	(ans (reverse (list "DNA-NT 0" "DNA-NT 1" "DNA-NT 2"
		   "DNA-NT 3" "DNA-NT 4" "DNA-NT 5"))));; we reverese since we are not doing set comparison we are doing list compairison so order matters TODO add set comparison fns
    (loop for i from 0 to 5 do
      (+ht ht (make-dna-nt)))
;;      (format t "~& ~A ~%" (alexandria:hash-table-keys ht))))

    (is equal ans (alexandria:hash-table-keys ht))))

(define-test "numtype-in-ht (typ ht)"
  (let* ((ht (make-hash-table)))
    (loop for i from 2 to 11 do
      (setf (gethash i ht) i))
    (is eq 10 (numtype-in-ht (type-of 2) ht))))

  
		       
