(in-package #:small-tests)




(define-test "Test make-dna-nt creates a DNA-NT CHEM-OBJ"
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
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn)))
    (is equal
	"1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 0.0 0.0"
	(oxdna-config nt :inc-headers nil))
    (is equal
	'("t = 0.0" "b = 1.0 1.0 1.0" "E = 0.0 0.0 0.0" "1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 0.0 0.0")
	(oxdna-config nt :inc-headers t))))



(define-test "Test DNA-NT:oxdna-topology single DNA-NT only"
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

(define-test "TEST: oxdna-config-string"
  (let* ((cm (v3 1 0 0))
	 (vbb (v3 0 1 0))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vbb vbb :vn vn))
	 (res (oxdna-config-string nt)))
    (of-type 'STRING res)
    (is equal "1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 0.0 0.0 0.0" res)))

(define-test "(oxdna-box-size nt :all)"
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
	 (ans-all '("t = 0.0" "b = 5.0 5.0 5.0" "E = 0.0 0.0 0.0"))
	 (ans-single (list "t = 0.0" "b = 4.0 4.0 4.0" "E = 0.0 0.0 0.0"))
	 (ans-b (list "t = 0.0" "b = 7.0 7.0 7.0" "E = 0.0 0.0 0.0")))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (is equal "" t)))


(define-test "TEST: (oxdna-config-header nt)"
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
	 (ans-all (list "t = 0.0" "b = 5.0 5.0 5.0" "E = 0.0 0.0 0.0"))
	 (ans-single (list "t = 0.0" "b = 1.0 1.0 1.0" "E = 0.0 0.0 0.0")) ; single nts get a box on 1nm^3 TODO choose good size
	 (ans-b (list "t = 0.0" "b = 7.0 7.0 7.0" "E = 0.0 0.0 0.0")))
    (connect-nts nt1 nt2 nt3 nt4 nt5 nt6)
    (define-test "(oxdna-config-header nt :all t)"
      (is equal ans-all (oxdna-config-header nt4 :all t)))
    (define-test "(oxdna-config-header nt :all nil)"
      (is equal ans-single (oxdna-config-header nt4 :all nil)))
    (define-test "(oxdna-config-header nt :all b) test b gets used over calc"
      (is equal ans-b (oxdna-config-header nt4 :all t :b (v3 7 7 7))))))



;;(remove-test "(oxdna-header nt :all nil)")

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
    
