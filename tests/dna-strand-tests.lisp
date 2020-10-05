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

;(remove-test "(add-nt dna-strand)")
(define-test "(add-nt dna-strand)" 
  (let* ((s (make-dna-strand))
	 (nt1 (make-dna-nt))
	 (nt2 (make-dna-nt))
	 (nt3 (make-dna-nt)))
    (is equal nil (5end s))
    (is equal nil (3end s))
    (add-nt s :nt nt1)
    (is equal nt1 (5end s :all nil))
    (is equal nt1 (3end s :all nil))
    (is equal (list nt1) (connected-nts (5end s)))
    (add-nt s :nt nt2)
    (is equal nt1 (5end s :all nil))
    (is equal nt2 (3end s :all nil))
    (is equal (list nt1 nt2) (connected-nts (5end s)))
    (add-nt s :nt nt3 :5end t)
    (is equal nt3 (5end s :all nil))
    (is equal nt2 (3end s :all nil))
    (is equal (list nt3 nt1 nt2) (connected-nts (5end s)))
  ;;todo more tests for multi-stranded cases
  ))
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







