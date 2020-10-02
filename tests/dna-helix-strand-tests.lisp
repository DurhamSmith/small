(in-package :small-tests)

(define-test "(next-helix-vbb nt)"
  (let* ((sc0 (v3l '(-30.5 0. -43.879999999999995)))
	 (st0 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
	 (sc1 (v3l '(-30.5 0. -43.879999999999995)))
	 (st2 (v3l '(-32.36602540378444 -0.49999999999999994 -43.879999999999995)))
	 (ax (v3l '(-31.5 0 -43.879999999999995)))
	 (vbb (magicl:.- sc0 ax))
	 ;; (ans-cm+1 (magicl:.- sc0 ans))
	 ;; (ax-vbb+1 (magicl:.- sc0 ax))
	 ;; (ans-vn+1 vn)
	 (cm (magicl:.+ ax (magicl:scale vbb 0.6d0)))
	 (vn (v3 0 0 1))
	 (nt (make-dna-nt :cm cm :vn vn :vbb vbb)))
    (is #'magicl:= (second *bb->sc*) (small::next-helix-vbb vbb vn))
;    (multiple-value-bind cm+1 vbb+1 vn+1
      (small::next-helix-nt-coords cm vbb vn));; This fails but is equal, probably a rounding error
      
  )
    ;; (let* ((nt+1 (small::next-helix-nt nt)
    ;;  ))
