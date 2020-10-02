(in-package #:small-tests)

(define-test "Test v3 creates a 3D vector with the correct type and shape"
  (let* ((x 1)
	 (y 2)
	 (z 3)
	 (type-spec '(double-float))
	 (v (v3 x y z))
	 (vx (x v))
	 (vy (y v))
	 (vz (z v)))
    (is = vx (coerce x type-spec))
    (is = vy (coerce y type-spec))
    (is = vz (coerce z type-spec))
    (is equal '(3 1) (magicl:shape v))))


(define-test "Test v3 accessor fns: x, y z"
  (let* ((v (v3 0 1 2)))
    (is = 0d0 (x v))
    (is = 1d0 (y v))
    (is = 2d0 (z v))))



(define-test "(on-v3-axis)"
  (let* ((v- (v3 -1 -2 -3))
	 (v0 (v3 0 0 0))
	 (v+ (v3 1 2 3))
	 (vecs (list v- v0 v+)))
    (is equal -1d0 (on-v3-axis #'min 'x vecs :by #'reduce))
    (is equal -2d0 (on-v3-axis #'min 'y vecs :by #'reduce))
    (is equal -3d0 (on-v3-axis #'min 'z vecs :by #'reduce))
    (is equal 1d0 (on-v3-axis #'max 'x vecs :by #'reduce))
    (is equal 2d0 (on-v3-axis #'max 'y vecs :by #'reduce))
    (is equal 3d0 (on-v3-axis #'max 'z vecs :by #'reduce))))


(define-test "(bounds)"
  ;; todo test ()  and (())  handling
  (let* ((v- (v3 -1 -2 -3))
	 (v0 (v3 0 0 0))
	 (v+ (v3 1 2 3))
	 (vecs (list v- v0 v+)))
    (multiple-value-bind (range min max)
	(bounds vecs)
      (is #'magicl:= (magicl:.- v+ v-) range)
      (is #'magicl:= v- min)
      (is #'magicl:= v+ max))))

    



(define-test "print-v3 outputs correctly"
  (is equal "1.0 2.0 3.0" (print-v3 (v3 1 2 3) :stream nil))
  (is equal "pre1.0 2.0 3.0app" (print-v3 (v3 1 2 3) :stream nil :prepend "pre" :append "app")))


(define-test "(rotation-matrix (axis theta))"
  (let* ((theta (/ pi 4))
	 (x (v3 1 0 0))
	 (y (v3 0 1 0))
	 (z (v3 0 0 1))
	 (xrot (magicl:from-list
		`( 1d0 0d0 0d0
		   0d0 ,(cos theta) ,(- (sin theta))
		   0d0 ,(sin theta) ,(cos theta))
			'(3 3)))
	 (yrot (magicl:from-list
			`(,(cos theta) 0d0 ,(sin theta)
			   0d0 1d0 0d0
			  ,(- (sin theta)) 0d0 ,(cos theta) )
			'(3 3)))
	 (zrot (magicl:from-list
			`(,(cos theta) ,(- (sin theta)) 0d0
			  ,(sin theta) ,(cos theta) 0d0
			  0d0 0d0 1d0)
			'(3 3))))
    (is #'magicl:= xrot (rotation-matrix x theta))
    (is #'magicl:= yrot (rotation-matrix y theta))
    (is #'magicl:= zrot (rotation-matrix z theta))))

(define-test "(rotate-vec v axis theta)"
  (let* ((theta (/ pi 2))
	 (x (v3 1 0 0))
	 (y (v3 0 1 0))
	 (z (v3 0 0 1)))
    (is #'magicl:= x (rotate-vec y z (/ pi -2)))
    (is #'magicl:= x (rotate-vec z y (/ pi 2)))
    (is #'magicl:= y (rotate-vec x z (/ pi 2)))
    (is #'magicl:= y (rotate-vec z x (/ pi -2)))
    (is #'magicl:= z (rotate-vec x y (/ pi -2)))
    (is #'magicl:= z (rotate-vec y x (/ pi 2)))))
