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



(define-test "Test DNA-NT:oxdna-config"
  :parent dna-nt-suite
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (is equal
	"1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 0.0 0.0"
	(oxdna-config nt))))



(define-test "Test DNA-NT:oxdna-topology single DNA-NT only"
  :parent dna-nt-suite
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn :base "G"))
	 (ans-top '("1 G -1 -1"))
	 (ans-header "1 1"))
    (multiple-value-bind (top header)
	(oxdna-topology nt :inc-headers nil)
      (is equal ans-top top)
      (is equal ans-header header))))


(define-test "Test DNA-NT:oxdna-topology traversing multiple nts"
  (let* ((v1 (v3 1 1 1))
	 (nt1 (make-dna-nt :base "G"))
	 (nt2 (make-dna-nt :base "C"))
	 (nt3 (make-dna-nt :base "G"))
	 (nt4 (make-dna-nt :base "T"))
	 (nt5 (make-dna-nt :base "T"))
	 (nt6 (make-dna-nt :base "G"))
	 (ans-top '("1 G -1 1" "1 C 0 2" "1 G 1 3" "1 T 2 4" "1 T 3 5" "1 G 4 -1"))
	 (ans-header "6 1"))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (multiple-value-bind (top header)
	(oxdna-topology nt4 :all t :inc-headers nil)
      (is equal ans-top top)
      (is equal ans-header header))))


(define-test "Test DNA-NT:connected-nts"
  (let* ((nt1 (make-dna-nt :base "G"))
	 (nt2 (make-dna-nt :base "C"))
	 (nt3 (make-dna-nt :base "G"))
	 (nt4 (make-dna-nt :base "T"))
	 (nt5 (make-dna-nt :base "T"))
	 (nt6 (make-dna-nt :base "G"))
	 (answer (list nt1 nt2 nt3 nt4 nt5 nt6)))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (is equal answer (connected-nts nt4))))




(define-test "Test DNA-NT:CONNECT"
  :parent dna-nt-suite
  (let* ((v (v3 1 1 1))
	 (-v (magicl:scale v -1))
	 (nt1 (make-dna-nt :cm v :vbb v :vn v))
	 (nt2 (make-dna-nt :cm -v :vbb -v :vn -v)))
    (isnt eq nt1 nt2)
    (connect nt1 nt2)
    (is eq nt2 (next nt1))))

(define-test "Test DNA-NT:COLLECT-NTs"
  (let* ((v1 (v3 1 1 1))
	 (v2 (magicl:scale v1 2))
	 (v3 (magicl:scale v1 3))
	 (v4 (magicl:scale v1 4))
	 (nt1 (make-dna-nt :cm v1 :vbb v1 :vn v1))
	 (nt2 (make-dna-nt :cm v2 :vbb v2 :vn v1))
	 (nt3 (make-dna-nt :cm v3 :vbb v3 :vn v1))
	 (nt4 (make-dna-nt :cm v4 :vbb v4 :vn v1)))
    (isnt eq nt2 (next nt1))
    (isnt eq nt3 (next nt2))
    (isnt eq nt4 (next nt3))
    (isnt eq nt1 (prev nt2))
    (isnt eq nt2 (prev nt3))
    (isnt eq nt3 (prev nt4))
    (is equal (list nt1 nt2 nt3 nt4)
	(connect-nts nt1 nt2 nt3 nt4))
    (is eq nt2 (next nt1))
    (is eq nt3 (next nt2))
    (is eq nt4 (next nt3))
    (is eq nt1 (prev nt2))
    (is eq nt2 (prev nt3))
    (is eq nt3 (prev nt4))))



(define-test "Test (oxdna-topology-from-seq ...)"
  (let* ((seq "GCGTTG")
	 (ans1 '("1 G -1 1" "1 C 0 2" "1 G 1 3" "1 T 2 4" "1 T 3 5" "1 G 4 -1"))
	 (ans2 '("1 G 0 11" "1 C 10 12" "1 G 11 13" "1 T 12 14" "1 T 13 15" "1 G 14 99"))
	 (ans3 '("2 G 0 11" "2 C 10 12" "2 G 11 13" "2 T 12 14" "2 T 13 15" "2 G 14 99"))
	 (ans-header "6 1"))
    (multiple-value-bind (lines header)
	(oxdna-topology-from-seq seq :inc-headers nil)
      (is equal ans1 lines)
      (is equal ans-header header))
    (multiple-value-bind (lines header)
	(oxdna-topology-from-seq seq :start 10 :prev 0 :next 99 :inc-headers nil)
      (is equal ans2 lines)
      (is equal ans-header header))
    (multiple-value-bind (lines header)
	(oxdna-topology-from-seq seq :prev 0 :start 10 :next 99 :strand-num 2 :inc-headers nil)
      (is equal "6 2" header)
      (is equal ans3 lines))
    (multiple-value-bind (lines header)
	(oxdna-topology-from-seq seq :inc-headers t)
      (is equal (append (list ans-header) ans1) lines)
      (is equal ans-header header))))


(define-test "TEST: (write-oxdna 'DNA-NT :all t)"
  (let* ((v1 (v3 1 1 1))
	 (v2 (magicl:scale v1 2))
	 (v3 (magicl:scale v1 3))
	 (v4 (magicl:scale v1 4))
	 (v5 (magicl:scale v1 5))
	 (v6 (magicl:scale v1 6))
	 (nt1 (make-dna-nt :cm v1 :vbb v1 :vn v1 :base "G")) 
	 (nt2 (make-dna-nt :cm v2 :vbb v2 :vn v2 :base "C"))
	 (nt3 (make-dna-nt :cm v3 :vbb v3 :vn v3 :base "G"))
	 (nt4 (make-dna-nt :cm v4 :vbb v4 :vn v4 :base "T"))
	 (nt5 (make-dna-nt :cm v5 :vbb v5 :vn v5 :base "T"))
	 (nt6 (make-dna-nt :cm v6 :vbb v6 :vn v6 :base "G")))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (write-oxdna nt4 :filename "write-oxdna-test")))

(define-test "TEST: oxdna->file"
  ;; TODO cm, vbb, vn are all in the same. Not a physical use case
  (skip "Not ready yet")
  (let* ((v1 (v3 1 1 1))
	 (v2 (magicl:scale v1 2))
	 (v3 (magicl:scale v1 3))
	 (v4 (magicl:scale v1 4))
	 (v5 (magicl:scale v1 5))
	 (v6 (magicl:scale v1 6))
	 (nt1 (make-dna-nt :cm v1 :vbb v1 :vn v1 :base "G")) 
	 (nt2 (make-dna-nt :cm v2 :vbb v2 :vn v2 :base "C"))
	 (nt3 (make-dna-nt :cm v3 :vbb v3 :vn v3 :base "G"))
	 (nt4 (make-dna-nt :cm v4 :vbb v4 :vn v4 :base "T"))
	 (nt5 (make-dna-nt :cm v5 :vbb v5 :vn v5 :base "T"))
	 (nt6 (make-dna-nt :cm v6 :vbb v6 :vn v6 :base "G"))
	 (config))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (oxdna->file "oxtest"
		 (mapcar #'oxdna-config (connected-nts nt4))
		 (oxdna-topology nt4 :all t))))
    
