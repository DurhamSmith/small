;;;; Lose nts when connectintg 5-5

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
  (break "~A" strands)
  (small::wmdna "a"
		(list
		 h1
		 p1
		 p2
		 p3
		  ))
  (format t "P1 5: ~A 3: ~A~%" (SMALL::base (SMALL::5nt p1)) (SMALL::base (SMALL::3nt p1)))
  (format t "P2 5: ~A 3: ~A~%" (SMALL::base (SMALL::5nt p2)) (SMALL::base (SMALL::3nt p2)))
  (format t "P3 5: ~A 3: ~A~%" (SMALL::base (SMALL::5nt p3)) (SMALL::base (SMALL::3nt p3)))
  )


 (sb-alien:alien-funcall
  (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))


(defun create-staple (scaff-spec)
  "Creates a partner for each scaff-obj"
  (let* ((stap-hels
	   (mapcar #'(lambda (obj-spec)
		       (staple-partner
			(getf obj-spec :obj)
			:start (getf obj-spec :start)
			:end (getf obj-spec :end)
			:from-3end (getf obj-spec :from-3end)
			))
		   scaff-spec)))
    (reduce #'(lambda (h1 h2)
		(break "~A ~A ~A" h1  h2 stap-hels)
		(when h2
		  (connect (5nt h1) (5nt h2))
		  h2)
		) stap-hels)
    (values stap-hels (connected-nts (5nt (first stap-hels))))))

