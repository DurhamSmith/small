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
      (is equal res2 res-tfms))))

(test "rotate then translate chem-obj" :report 'interactive)
(define-test "rotate then translate chem-obj"
  (let* ((obj (make-instance 'SMALL::chem-obj))       
	 (x (SMALL::v3 1 0 0))
	 (v (SMALL::v3 0 0 1))
	 (mat (MAGICL:from-list  ;90deg rotation around xy axis
	       `( 0d0 -1d0 0d0
		  1d0 0d0 0d0
		  0.0d0 0d0 0d0)
	       '(3 3)))
	 (res1 (list (cons "rotate" mat)))
	 (res2 (list (cons "translate" v)
		     (cons "rotate" mat))))
    (multiple-value-bind (res-obj1 res-tfms1)
	(SMALL::rotate-obj obj mat)
      (is eq obj res-obj1)
      (is equal res1 res-tfms1))
    (multiple-value-bind (res-obj2 res-tfms2)
	(SMALL::translate-obj obj v)
      (is eq obj res-obj2)
      (is equal res2 res-tfms2))
    (is-close (v3 0 1 1)
	      (SMALL::apply-transformations obj x))))



(test "(apply-transformation tfm v)" :report 'interactive)
(define-test "(apply-transformation tfm v)"
  (let* ((obj (make-instance 'SMALL::chem-obj))       
	 (x (SMALL::v3 1 0 0))
	 (v (SMALL::v3 0 0 1))
	 (mat (MAGICL:from-list  ;90deg rotation around xy axis
	       `( 0d0 -1d0 0d0
		  1d0 0d0 0d0
		  0.0d0 0d0 0d0)
	       '(3 3)))
	 (rot-tfm (cons "rotate" mat))
	 (trans-tfm (cons "translate" v))
	 (tfms (list trans-tfm rot-tfm)))
    (is-close (v3 0 1 0) (small::apply-transformation rot-tfm x))
    (is-close (v3 0 1 1) (small::apply-transformation trans-tfm ;; Rotate x to y then add +1z
						      (small::apply-transformation rot-tfm x)))))
