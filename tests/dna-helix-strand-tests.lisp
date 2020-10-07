(in-package :small-tests)

(defun is-close (v1 v2 &key (e 1e-4))
  (is eq T (small::vec-close v1 v2 :e e)))

(define-test "(next-helix-vbb nt)"
  (let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
	 (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
	 (sc1 (v3l '(-30.675703406387925 0.5661582161900746 -43.54)))
	 (st1 (v3l '(-31.5 0 -43.54)))
	 (ax0 (v3l '(-31.5 0 -43.879999999999995)))
	 (ax1 (v3l '(-31.5 0 -43.54)))
	 (vbb (magicl:.- sc0 ax0))
	 (cm (magicl:.+ ax0 (magicl:scale vbb 0.6d0)))
	 (vn (v3 0 0 1))
	 (vbb+1 (magicl:.- sc1 ax1))
	 (vcm-offset (magicl:scale vbb+1 0.6d0))
	 (cm+1 (magicl:.+ ax1 vcm-offset))
	 (vn+1 (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vn vn :vbb vbb)))
    (is-close (second *bb->sc*) (small::next-helix-vbb vbb vn))
    (multiple-value-bind (res-cm+1 res-vbb+1 res-vn+1)
	(small::next-helix-nt-coords cm vbb vn)
      (is-close cm+1 res-cm+1)
      ;; This fails but is equal, probably a rounding error
      (is-close vbb+1 res-vbb+1)
      (is-close vn+1 res-vn+1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; THIS writes the oxdna files for the fist row of the tiles helix. No tests right now just inspection ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
;;        (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
;;        (ax0 (v3l '(-31.5 0 -43.879999999999995)))
;;        (vbb (magicl:.- sc0 ax0))
;;        (cm (magicl:.+ ax0 (magicl:scale vbb 0.6d0)))
;;        (vn (v3 0 0 1))
;;        (nt (make-dna-nt :cm cm :vn vn :vbb vbb))
;;        (nts (connect-nts (loop for i from 1 to 33 collect
;; 						  (progn
;; 						    (setf nt (small::next-helix-nt nt)))))))
;;   (write-oxdna nt :filename "row1_tmp"))





(let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
       (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
       (ax0 (v3l '(-31.5 0 -43.879999999999995)))
       (vbb (as-unit-vec (magicl:.- sc0 ax0)))
       (cm (magicl:.+ ax0 (magicl:scale vbb 0.6d0)))
       (vn (v3 0 0 1))
       (hs (SMALL::helix-strand ax0 vn vbb 33)))
  (write-oxdna (SMALL::5nt hs) :filename "helgun"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Tests creating and writing a partner strand ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
       (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
       (ax0 (v3l '(-31.5 0 -43.879999999999995)))
       (vbb (magicl:.- sc0 ax0))
       (cm (magicl:.+ ax0 (magicl:scale vbb 0.6d0)))
       (vn (v3 0 0 1))
       (nt (make-dna-nt :cm cm :vn vn :vbb vbb))
       (nts (connect-nts (loop for i from 1 to 33 collect
						  (progn
						    (setf nt (small::next-helix-nt nt))))))
       (pts (connect-nts (mapcar #'small::partner (reverse nts))))
       (pt (first pts)))
  (write-oxdna pt :filename "pt"))
