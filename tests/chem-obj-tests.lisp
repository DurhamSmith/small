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
	  (obj (make-instance 'SMALL::chem-obj)))
      (multiple-value-bind (res-obj res-tfms)
	  (SMALL::add-transformation obj v)
	(is eq obj res-obj)
	(is-close v (car res-tfms)))))
      ;; (is-values (SMALL::add-transformation obj v)
      ;; 	(eq obj)
      ;; 	(is-close v))
	))
