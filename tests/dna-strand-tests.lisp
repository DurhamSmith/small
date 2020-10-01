(in-package :small-tests)


(define-test "(5end dna-strand)"
  (let* ((s (make-dna-strand))
	 (cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt1 (make-dna-nt :cm cm :vbb vbb :vn vn))
	 (nt2 (make-dna-nt))
	 (nt3 (make-dna-nt))
	 (nt4 (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (setf (small::5nt s) nt1)
    (multiple-value-bind (nt coords)
	(5end s)
      (is eq nt1 nt)
      (is #'magicl:= cm coords)))
  ;; TODO check 3end and length are preserved
  )

(define-test "(3end dna-strand)"
  (let* ((s (make-dna-strand))
	 (cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt1 (make-dna-nt :cm cm :vbb vbb :vn vn))
	 (nt2 (make-dna-nt))
	 (nt3 (make-dna-nt))
	 (nt4 (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (setf (small::3nt s) nt4)
    (multiple-value-bind (nt coords)
	(3end s)
      (is eq nt4 nt)
      (is #'magicl:= cm coords)))
  ;; TODO check 3end and length are preserved
  )


;(remove-test "(grow dna-strand)")
;; (define-test "(grow dna-strand)"
;;   (let* ((s (make-dna-strand))
;; 	 (nt1 (make-dna-nt))
;; 	 (nt2 (make-dna-nt))
;; 	 (nt3 (make-dna-nt))
;; 	 (nt4 (make-dna-nt)))
;;     (skip "Add-nt not implemented"
;;       (is eq nil (nts s))
;;       (grow s (list nt1 nt2 nt3 nt4))
;;       (is eq nt1 (nth-nt s 0))
;;       (is eq nt2 (nth-nt s 1))
;;       (is eq nt3 (nth-nt s 2))
;;       (is eq nt4 (nth-nt s 3))
;;       (is eq nt1 (5end s))
;;       (is eq nt4 (3end s))
;;       (is eq (list nt1 nt2 nt3 nt4)
;; 	  (nts s :all nil)))))


;;(remove-test "(add-nt dna-strand)")
;; (define-test "(add-nt dna-strand)" 
;;   (let* ((s (make-dna-strand))
;; 	 (nt1 (make-dna-nt))
;; 	 (nt2 (make-dna-nt))
;; 	 (nt3 (make-dna-nt)))
;;     (skip "Not implemented"
;;       (is eq nil (5end))
;;       (is eq nil (3end))
;;       (add-nt s nt1)
;;       (is eq nt1 (5end s :nt t))
;;       (is eq nt1 (3end s :nt t))
;;       (is eq (list nt1) (nts s))
;;       (add-nt s nt2)
;;       (is eq nt1 (5end s :nt t))
;;       (is eq nt2 (3end s :nt t))
;;       (is eq (list nt1 nt2) (nts s))
;;       (add-nt s nt3 :5end t)
;;       (is eq nt3 (5end s :nt t))
;;       (is eq nt2 (3end s :nt t))
;;       (is eq (list nt3 nt1 nt2) (nts s)))
;;     ;;TODO more tests for multi-stranded cases
;;     ))
    
    
	
   
    
