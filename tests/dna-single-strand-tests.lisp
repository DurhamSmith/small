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






(define-test "single-strand (coords0 vaxis vbb0 len)"
  (let* ((p1 (v3 2 0 0))
	 (p2 (v3 4.20 0 0)) ;; should need 4nts of 0, .4nt spacing and have 0.1nm spacing and there is an offset of -1 nt
	 (answer-cms (list
		      (v3 2d0 0d0 0.6d0) 
		      (v3 2.4d0 0d0 0.6d0)
		      (v3 2.8d0 0d0 0.6d0)
		      (v3 3.2d0 0d0 0.6d0)
		      (v3 4.0d0 0d0 0.6d0)))
	 (answer-vbb (v3 0 0 1))
	 (answer-vn (v3 1 0 0)))
    (multiple-value-bind (strand nts)
	(small::single-strand  (v3 2 0 0) (v3 1 0 0) (v3 0 0 1) 4)
      (of-type SMALL:DNA-SINGLE-STRAND strand)
;      (break "~A " (SMALL::cm (first nts)))
      (mapcar #'(lambda (cm nt)
		  (is-close cm (SMALL::cm nt))
		  (is-close answer-vbb (SMALL::vbb nt))
		  (is-close answer-vn (SMALL::vn nt)))
	      answer-cms nts)))
  )
      ;; strand
      ;; nts
      ;; (write-oxdna (first nts) :filename "pfl")
      ;; ))


;(let* 


