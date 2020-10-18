(in-package #:small-tests)

;; (define-test "Test chem-obj class"
  
;;   (define-test "Test connecting two chem-objs without specialized connect methods fail"
;;     (skip "Not ready yet"
;;       (is = 0 1)
;;       (is = 0 1)
;;       (is = 0 1))))



(SMALL::vec-close (SMALL::v3 1 0 0) (SMALL::v3 1 0 0))

(test "(add-transformation obj ...)")
(define-test "(add-transformation obj ...)"
  (let ((v (SMALL::v3 1 0 0))
	(mat (MAGICL:from-list
	      `( 1d0 0d0 0d0
		 0.5d0 5d0 0d0
		 0.0d0 0d0 5d0)
	      '(3 3)))
	(obj (make-instance 'SMALL::chem-obj)))
    (multiple-value-bind (res-obj res-tfms)
	(SMALL::add-transformation obj v)
      (is eq obj res-obj)
      (is-close v (car res-tfms)))
    (multiple-value-bind (res-obj res-tfms)
	(SMALL::add-transformation obj mat)
      (is eq obj res-obj)
      (is-close mat (car res-tfms)))))  ;TODO: Check that vec-close / is-close correctly compares matrices


(test "(translate-obj obj v)" :report 'interactive)
(define-test "(translate-obj obj v)"
  (let* ((v (SMALL::v3 1 0 0))       
	 (obj (make-instance 'SMALL::chem-obj))
	 (res1 (list (cons "translate" v)))
	 (v2 (SMALL::v3 2 0 0))       
	 (res2 (list (cons "translate" v2)
		     (cons "translate" v))))
    (multiple-value-bind (res-obj res-tfms)
	(SMALL::translate-obj obj v)
      (is eq obj res-obj)
      (is equal res1 res-tfms))
    (multiple-value-bind (res-obj res-tfms)
	(SMALL::translate-obj obj v2)
      (is eq obj res-obj)
      (is equal res2 res-tfms))
    ))
