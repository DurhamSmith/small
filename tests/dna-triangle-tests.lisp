(in-package #:small)


(wmdna "tall" (let ((t1  (make-instance 'dna-triangle)))
	       (list
		(first (subseq (alexandria::flatten (all-to-write t1)) 0 5))
		(last (subseq (alexandria::flatten (all-to-write t1)) 1 5)))))



(wmdna "tall" (let* ((t1  (make-instance 'dna-triangle))
		     (af (alexandria::flatten (all-to-write t1))))
		(list
		(first af)
		(subseq af 1 4)
		(subseq af 5 34)
		(subseq af 55 56))))

(wmdna "tall" (let ((t1  (make-instance 'dna-triangle)))
		(subseq (alexandria::flatten (all-to-write t1)) 0 5)))

(wmdna "1-4" (let* ((t1  (make-instance 'dna-triangle))
		     (af (alexandria::flatten (all-to-write t1))))
		(list
		(first af)
		(subseq af 1 4))))

(wmdna "tall" (let* ((t1  (make-instance 'dna-triangle))
		     (af (alexandria::flatten (all-to-write t1))))
		(list
		(first af)
		(subseq af 4 5)
		(subseq af 1 2))))

(wmdna "4" (let* ((t1  (make-instance 'dna-triangle))
		     (af (alexandria::flatten (all-to-write t1))))
		(list
		(first af)
		(subseq af 4 5))))

(wmdna "1" (let* ((t1  (make-instance 'dna-triangle))
		     (af (alexandria::flatten (all-to-write t1))))
		(list
		(first af)
		(subseq af 1 2))))

(let* ((t1  (make-instance 'dna-triangle))
       (af (alexandria::flatten (all-to-write t1)))
       (1-3 (3nt (first (subseq af 1 2))))
       (1-5 (5nt (first (subseq af 1 2))))
       (4-3 (3nt (first (subseq af 4 5))))
       (4-5 (5nt (first (subseq af 4 5)))))

  (mapcar #'partner (list 1-3 1-5 4-3 4-5)))
       
       (1-(subseq af 1 2))))
  1-3)
  
  


