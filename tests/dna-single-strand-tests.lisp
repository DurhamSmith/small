(in-package :small-tests)


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
