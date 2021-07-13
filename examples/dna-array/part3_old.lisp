(in-package :small)
(load "~/quicklisp/local-projects/small/examples/dna-array/part2.lisp")

(load "~/quicklisp/local-projects/small/autostaple.lisp")


(let* ((tile (make-instance 'dna-tile))
       (pts (mapcar #'make-partner (connected-nts (5nt tz))))
       (ptsc (mapcar #'connect pts (cdr pts)))
       (exts (mapcar #'(lambda (nt)
				    (surface-extension-strand nt 30))
				(bb-pointing pts)
				)))
  (update-scaffold-bases tile *m13mp18*)
  (wmdna "tile-v6" tile (mapcar #'(lambda (nt)
				    (surface-extension-strand nt 30))
				(bb-pointing pts)
				)))
			 
(length
 (let* ((tile (make-instance 'dna-tile))
       (pts (mapcar #'make-partner (connected-nts (5nt tz))))
       (ptsc (mapcar #'connect pts (cdr pts))))
  (bb-pointing pts)))



(find-obj-with-props (tile-helices tz)
					`((:i . 1)))


(defun bb-pointing (nts &key (v (v3 0 1 0)) (threshold (- 1 (deg->rad 0.07))))  
  (remove nil
	  (mapcar #'(lambda (nt)
		      (let ((x (magicl:tref (magicl:@ (magicl::transpose (vbb nt)) v) 0 0)))
			(if (> x threshold)
			    nt
			    nil)))
		  nts)))

(defun finddna (strand)
  (let* ((nts (strand-nts strand))
	 (pts (mapcar #'make-partner nts))
	 (exts (mapcar #'(lambda (nt)
			   (surface-extension-strand nt 30))
		       (bb-pointing pts)
		       )))
    exts))



(remove nil (mapcar #'finddna (triangle-helices tz 1)))

(let* ((tz (make-instance 'dna-tile))
       (pts (mapcar #'make-partner (connected-nts (5nt tz))))
       (ptsc (mapcar #'connect pts (cdr pts))))
       
  (wmdna
   "1"
   tz
   (first pts)
   (remove nil
	   (list
	    (first (tri-row-exts tz 4 12))
	    (tri-row-exts tz 4 15)
	    (tri-row-exts tz 4 7)
	    
	    (tri-row-exts tz 1 9)
	    (tri-row-exts tz 1 13)
	    (second (tri-row-exts tz 1 20))
	    
	    (second (tri-row-exts tz 3 4))
	    (tri-row-exts tz 3 9)
	    (tri-row-exts tz 3 15)

	    (third (tri-row-exts tz 2 10))
	    (third (tri-row-exts tz 2 12))
	    
	    ))))


(let* ((tz (make-instance 'dna-tile))
       (pts (mapcar #'make-partner (connected-nts (5nt tz))))
       (ptsc (mapcar #'connect pts (cdr pts))))
       
  (wmdna
   "2"
   tz
   (first pts)
   (remove nil
	   (list
	    (first (tri-row-exts tz 4 12))
	    (tri-row-exts tz 4 15)
	    (tri-row-exts tz 4 7)
	    
	    (tri-row-exts tz 1 9)
	    (tri-row-exts tz 1 13)
	    (second (tri-row-exts tz 1 20))
	    
	    (second (tri-row-exts tz 3 4))
	    (tri-row-exts tz 3 9)
	    (tri-row-exts tz 3 13)

	    (third (tri-row-exts tz 2 10))
	    (third (tri-row-exts tz 2 12))
	    
	    ))))


(defun tri-row-exts (tile k i)
  (let ((hel (find-obj-with-props
	      (triangle-helices tile k)
	      `((:i . ,i)))))
    (finddna hel)))



(let* ((tile (make-instance 'dna-tile))
       (pts (mapcar #'make-partner (connected-nts (5nt tz))))
       (ptsc (mapcar #'connect pts (cdr pts)))
       (exts (remove nil (mapcar #'finddna
				 (list
				  (find-obj-with-props
				   (triangle-helices tile 1)
				   `((:i . 2))))))))
  (finddna

   (find-obj-with-props
   (triangle-helices tile 1)
   `((:i . 3)))))

  (update-scaffold-bases tile *m13mp18*)
  (wmdna "tile-v6" tile exts))

(length (remove nil (mapcar #'finddna (tile-helices tz))))

		   
	 
	 

  



(defun triangle-helices (tile k)
  (remove-if-not #'(lambda (x)
		     (typep x 'dna-helix-strand))
		 (scaffold (get-triangle tile k))))

		 
  

(setq tz (make-instance 'dna-tile))
(defun tile-helices (tile)
  (remove-if-not #'(lambda (x)
		     (typep x 'dna-helix-strand))
		 (alexandria:flatten
		  (mapcar #'scaffold (remove-if-not #'(lambda (x)
							(typep x 'dna-triangle))
						    (scaffold tile))))))
(tile-helices tz)

				    
				      
  
(connected-nts (5nt tz))
(reduce #'connect (mapcar #'make-partner (connected-nts (5nt tz))))

(defun surface-extension-strand (nt num-nts &key
					      (vaxis (v3 0 1 0))
					      (ext-dist (v3 0 0 0));(v3 0 (+ *helix-radius* *helix-cm-offset*) 0))
					      (vbb (v3 1 0 0)))
  "Takes a NT, a direction of the axis and the number of nts and returns a DNA-HELIX-STRAND that forms an extension above the strand. Coords = cm of nt + ext-dist"
  (let* ((ext-cm (MAGICL:.+ (cm->bb (cm nt) (vbb nt)) ext-dist))
	 (ext (helix-strand ext-cm vaxis vbb num-nts)))
    ext))

(p
