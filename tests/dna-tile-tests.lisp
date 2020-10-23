(in-package #:small-tests)


(let* ((tile  (SMALL::make-dna-tile))
       (t2  (SMALL::make-dna-tile))
       (trans-vec (SMALL::v3 (/ SMALL::*w* 2) (/ SMALL::*w* 2) 0))
       (rot-mat (small::rotation-matrix (v3 0 0 1) (/ pi 2))))
					;(break (first (SMALL::scaffold tile)))
  ;; (SMALL::translate-obj tile trans-vec)
  ;; (break "~A" (small::all-tfms (SMALL::5nt (first (SMALL::scaffold tile)))))
  ;; (break "~A" (small::vbb (SMALL::5nt (first (SMALL::scaffold tile)))))
  ;; (small::wmdna "t1" (append
  ;; 		 (list (first (SMALL::scaffold tile)))
  ;; 		 (SMALL::edge-staples tile)))
  (SMALL::rotate-obj t2 rot-mat)
  (SMALL::translate-obj t2 trans-vec)
  ;(break "~A" (small::all-tfms (SMALL::5nt (first (SMALL::scaffold tile)))))
  (small::wmdna "2t" (append
		      (list (first (SMALL::scaffold tile)))
		      (SMALL::edge-staples tile)
		      (list (first (SMALL::scaffold t2)))
		      (SMALL::edge-staples t2)))
  ;; (break (SMALL::edge-staples tile))
 )




(let* ((tile  (SMALL::make-dna-tile))
       (t2  (SMALL::make-dna-tile))
       (trans-vec (SMALL::v3 (/ SMALL::*w* 2) (/ SMALL::*w* 2) 0))
       (rot-mat (small::rotation-matrix (v3 0 0 1) (/ pi 2))))
  (SMALL::rotate-obj t2 rot-mat)
  (SMALL::translate-obj t2 trans-vec)
  (member (cons :i 1) (alexandria::hash-table-alist (SMALL::props (first (SMALL::scaffold t2)))))
  (alexandria::hash-table-alist (SMALL::props (first (SMALL::scaffold t2))))
  (gethash (car '(:i . 1)) (SMALL::props (first (SMALL::scaffold t2))))
  (small::has-props (first (SMALL::scaffold t2))
		    '((:i . 1) (:k . 1))))
  (break t2)
)



  ;; (small::wmdna "2t" (append
  ;; 		      (list (first (SMALL::scaffold tile)))
  ;; 		      (SMALL::edge-staples tile)
  ;; 		      (list (first (SMALL::scaffold t2)))
  ;; 		      (SMALL::edge-staples t2)))
  ;; (break (SMALL::edge-staples tile))
  )


  (break (SMALL::edge-staples tile)))



;(break h)

       
  ;; (small::wmdna "tt"
  ;; 		(list
  ;; 		 (scaffold tile)
  ;; 		 (edge-staples tile)))





(getf '('obj 1  :start 0 :end 16  ) :obj)
 ;; Tests creating staples kind of
(let* ((tile (SMALL::scaffold (SMALL::make-dna-tile)))
       (h1 (first tile))
       (h2 (second tile))
       (h3 (fourth tile))
       p1 p2 strands)
  (setf strands (SMALL::create-staple `((:obj ,h1  :start 0 :end 16  :from-3end t)
					(:obj ,h2  :start 0 :end 16  :from-3end nil)
					 (:obj ,h3  :start 0 :end 16  :from-3end t)
					)))
  (setf p1 (first strands))
  (setf p2 (second strands))
  (setf p3 (third strands))
  (format t "~A" (list
		 h1
		 p1
		 p2
		  p3
		  ))
  (small::wmdna "stapit"
		(list
		 h1
		 p1
		 p2
		  p3
		  )))





(let* ((tile (SMALL::make-dna-tile))
       (scaf-hels (filter #'(lambda (x)
			      (when (typep x 'dna-helix-strand)
				x))			      
			  (SMALL::scaffold tile))))
  scaf-hels)


  )  scaf-hels)
       staples)
  (b
  (setf strands (SMALL::create-staple `((:obj ,h1  :start 0 :end 16  :from-3end t)
					(:obj ,h2  :start 0 :end 16  :from-3end nil)
					(:obj ,h3  :start 0 :end 16  :from-3end t)
					)))
  (small::wmdna "stapit"
		(list
		 h1
		 p1
		 p2
		 p3
		 ))))

;;ali jabi
;;
	
  (defun filter (fn 1st)
    (let ((ace nil))
      (dolist (x 1st)
	(let ((val (funcall fn x)))
	  (if val (push val ace))))
      (nreverse ace)))




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
(car (first '(1 2 3 4 5)))

(let* ((strands '())
       (scaff-nts '()))
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (multiple-value-bind (hel nts)
	  (SMALL::scaffold-helix k i)
	(unless  (> 1 (length strands))
					;	  (break "x ~A y ~A" (first strands) strands);(small::3nt (car (last strands))) hel)
	  (connect (first strands) hel))
	(push hel strands)
	(push nts scaff-nts))
      (when (evenp i) ;; add scaffold-loops
	(multiple-value-bind (scaff-loop nts)
	    (SMALL::scaffold-loop k i)
					;	      (break "scloop ~A fs ~A" (small::5nt scaff-loop) (first strands))
	  (connect (first strands) scaff-loop)
	  (push scaff-loop strands)
	  (push nts scaff-nts)))))
					;	(small::write-oxdna (small::5nt hel) :filename (format nil "helix-~A-~A" k i)))))
  (setf strands (reverse strands))
  (format t "~A" strands)
  (small::write-oxdna (SMALL::5nt (first strands)) :filename "fulltile"))
;; (mapcar #'(lambda (strand)	      
;; 	      (small::write-oxdna (small::5nt strand) :filename (format nil "helix-~A-~A" k i)))
;; 	  strands))

;;(setf scaff-nts (connect-nts (reverse scaff-nts)))
;;(small::write-oxdna (first scaff-nts) :filename "scaff-2"))










(DI *tile-scaffolds* 1 2 1)
(small::scaffold-coords-1 2 1)

(define-test "scaffold-coords-1 (i j &key cm)"
      (skip "Takes long"
  (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do

	(is-close (DI *tile-scaffolds* 1 i j)
		  (small::scaffold-coords-1 i j))))))

(define-test "scaffold-coords (k i j &key cm)"
        (skip "Takes long"
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (loop for j from 1 to (SMALL::ai i) do

	  (is-close (DI *tile-scaffolds* k i j)
		    (small::scaffold-coords k i j)))))))


(define-test "staple-coords-1 (i j &key cm)"
  (skip "Takes long"
    (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do
	(is-close (DI *tile-staples* 1 i j)
		  (small::staple-coords-1 i j))
	))))


(define-test "staple-coords (k i j &key cm)"
    (skip "Takes long"
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (loop for j from 1 to (SMALL::ai i) do

	  (is-close (DI *tile-staples* k i j)
		    (small::staple-coords k i j)))))))

(define-test "helix-axis-coords-1 (i j &key cm)"
  (skip "Takes long"
    (loop for i from 1 to 22 do
    (loop for j from 1 to (SMALL::ai i) do    
	(is-close (DI *tile-axes* 1 i j)
		  (small::helix-axis-coords-1 i j))))))


(define-test "helix-axis-coords (k i j &key cm)"
    (skip "Takes long"
  (loop for k from 1 to 4 do
    (loop for i from 1 to 22 do
      (loop for j from 1 to (SMALL::ai i) do
	  (is-close (DI *tile-axes* k i j)
		    (small::helix-axis-coords k i j)))))))

