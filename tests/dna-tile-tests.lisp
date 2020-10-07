(in-package #:small-tests)




;; (defun make-tile-scaffold ()
;;   )





;; (let* ((nts (alexandria:flatten
;; 	     (loop
;; 	       for k from 1 to 4
;; 	       collect
;; 	       (loop
;; 		 for i from 1 to 22
;; 		 collect
;; 		 (loop
;; 		   for j from 1 to (small::ai i)
;; 		   collect
;; 		   (if (oddp i)
;; 		       (small::make-dna-nt :cm (small::scaffold-coords k i j :cm t)
;; 					   :vbb (small:as-unit-vec
;; 						 (magicl:.- (small::scaffold-coords k i j :cm nil)
;; 							    (small::helix-axis-coords k i j)))
					   
;; 					   :vn (small:as-unit-vec (if (oddp i)
;; 								      (magicl:.- (small::helix-axis-coords k i 1)
;; 										 (small::helix-axis-coords k i 2))
;; 								      (magicl:.- (small::helix-axis-coords k i 2)
;; 										 (small::helix-axis-coords k i 1)))))
;; 		       (small::make-dna-nt :cm (small::scaffold-coords k i (- (small::ai i) j) :cm t)
;; 					   :vbb (small:as-unit-vec
;; 						 (magicl:.- (small::scaffold-coords k i (- (small::ai i) j) :cm nil)
;; 							    (small::helix-axis-coords k i (- (small::ai i) j))))
;; 					   :vn (small:as-unit-vec (if (oddp i)
;; 								      (magicl:.- (small::helix-axis-coords k i 1)
;; 										 (small::helix-axis-coords k i 2))
;; 								      (magicl:.- (small::helix-axis-coords k i 2)
;; 										 (small::helix-axis-coords k i 1)))))
;; 		       ))))))
;;        (pts (reverse nts)))
;;   pts

;;   ;; (break "nts ~A ~%"  (subseq nts 0 10))
;;   ;;(break "pts ~A"  (subseq pts 0 10))
;;   (small::connect-nts nts)
;;   ;;(small::connect-nts pts)
;;   ;;(break "~A~%~A" (first pts) (last nts))
;;   ;;(break "~A~%~A~%~A" (small::cm (first nts)) (small::vbb (first nts)) (small::vn (first nts)))
;;   (small::write-oxdna (first nts) :filename "scaff"))
;; ;;(small::write-oxdna (first pts) :filename "stap"))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		;  Try to create tile from primatives ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (car (first '(1 2 3 4 5)))

;; (let* ((strands '())
;;        (scaff-nts '()))
;;   (loop for k from 1 to 4 do
;;     (loop for i from 1 to 22 do
;;       (multiple-value-bind (hel nts)
;; 	  (SMALL::scaffold-helix k i)
;; 	(unless  (> 1 (length strands))
;; ;	  (break "x ~A y ~A" (first strands) strands);(small::3nt (car (last strands))) hel)
;; 	  (connect (first strands) hel))
;; 	(push hel strands)
;; 	(push nts scaff-nts))
;;       (when (evenp i) ;; add scaffold-loops
;; 	(multiple-value-bind (scaff-loop nts)
;; 	    (SMALL::scaffold-loop k i)
;; ;	      (break "scloop ~A fs ~A" (small::5nt scaff-loop) (first strands))
;; 	  (connect (first strands) scaff-loop)
;; 	  (push scaff-loop strands)
;; 	  (push nts scaff-nts)))))
;; ;	(small::write-oxdna (small::5nt hel) :filename (format nil "helix-~A-~A" k i)))))
;;   (setf strands (reverse strands))
;;   (format t "~A" strands)
;;   (small::write-oxdna (SMALL::5nt (first strands)) :filename "fulltile"))
;;   ;; (mapcar #'(lambda (strand)	      
;;   ;; 	      (small::write-oxdna (small::5nt strand) :filename (format nil "helix-~A-~A" k i)))
;;   ;; 	  strands))
	      
;;   ;;(setf scaff-nts (connect-nts (reverse scaff-nts)))
;;   ;;(small::write-oxdna (first scaff-nts) :filename "scaff-2"))
  


      
(defun DI (data k i j)
  "Data index"
  (nth (- j 1) (nth (- i 1) (nth (- k 1) data))))

(DI *tile-scaffolds* 1 2 1)
(small::scaffold-coords-1 2 1)

(define-test "scaffold-coords-1 (i j &key cm)"
  (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do
      (is-close (v3l (DI *tile-scaffolds* 1 i j))
		(small::scaffold-coords-1 i j)))))

(define-test "scaffold-coords (k i j &key cm)"
(loop for k from 1 to 4 do
  (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do
      (is-close (v3l (DI *tile-scaffolds* k i j))
		(small::scaffold-coords k i j))))))


;; (define-test "scaffold-coords (k i j &key cm)"
;; ;  (loop for k from 1 to 3
;;   (loop for i from 1 to 22 do
;;     (loop for j from 1 to (SMALL::ai i) do
;;       (is-close (v3l (DI *tile-scaffolds* 1 i j))
;; 		(small::scaffold-coords-1 i j)))))



(define-test "staple-coords-1 (i j &key cm)"
  (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do
      (is-close (v3l (DI *tile-staples* 1 i j))
		(small::staple-coords-1 i j)))))


(define-test "staple-coords (k i j &key cm)"
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (loop for j from 1 to (SMALL::ai i) do
	(is-close (v3l (DI *tile-staples* k i j))
		  (small::staple-coords k i j))))))
  
