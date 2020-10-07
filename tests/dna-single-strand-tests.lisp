(in-package :small-tests)


(define-test "(ss-bridging-len p1 p1)"
  )


(define-test "(bridging-single-strand p1 p2)"
  
  (let* ((p1 (v3 2 0 0))
	 (p2 (v3 4.20 0 0))) ;; should need 4nts of 0, .4nt spacing and have 0.1nm spacing and there is an offset of -1 nt
    (multiple-value-bind (strand len)
	(small::bridging-single-strand p1 p2 (v3 1 0 0))
      (is = 4 len)
      (of-type 'SMALL::DNA-SINGLE-STRAND strand)
      (write-oxdna (small::5nt strand) :filename "sss_tmp")
      ))
  )
;	 (is = 5 (length (strand-nts strand)))
;	 (of-type small::dna-single-strand (first (strand-nts strand))))))))
	 

;;;; This seems to pass but I need a better way of testing it

(let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
       (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
       (ax0 (v3l '(-31.5 0 -43.879999999999995)))
       (vbb (magicl:.- sc0 ax0))
       (cm (magicl:.+ ax0 (magicl:scale vbb 0.6d0)))
       (vn (v3 0 0 1))
       (nt (make-dna-nt :cm cm :vn vn :vbb vbb))
       (nts (connect-nts (loop for i from 1 to 33 collect
						  (progn
						    (setf nt (small::next-single-strand-nt nt)))))))
  (write-oxdna nt :filename "strand_tmp"))






(let* ((p1 (v3 2 0 0))
	 (p2 (v3 4.20 0 0))) ;; should need 4nts of 0, .4nt spacing and have 0.1nm spacing and there is an offset of -1 nt
    (multiple-value-bind (strand nts)
	(small::single-strand  (v3 0 0 0) (v3 1 0 0) (v3 0 0 1) 10)
      strand
      nts
      (write-oxdna (first nts) :filename "pfl")
      ))
