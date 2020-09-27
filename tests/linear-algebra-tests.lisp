(in-package #:small-tests)

(define-test "Test v3 creates a 3D vector with the correct type and shape"
  (let* ((x 1)
	 (y 2)
	 (z 3)
	 (type-spec '(double-float))
	 (v (v3 x y z))
	 (vx (tref v3 0))
	 (vy (tref v3 1))
	 (vz (tref v3 2)))
    (is = vx (coerce x type-spec))
    (is = vy (coerce y type-spec))
    (is = vz (coerce z type-spec))
    (is = (shape v) 3)))

(define-test "Test v3 accessor fns: x, y z"
  (let* ((v (v3 0 1 2)))
    (is = 0d0 (x v))
    (is = 1d0 (y v))
    (is = 2d0 (z v))))

(define-test "print-v3 outputs correctly"
  (is equal "1.0 2.0 3.0" (print-v3 (v3 1 2 3) :stream nil))
  (is equal "pre1.0 2.0 3.0app" (print-v3 (v3 1 2 3) :stream nil :prepend "pre" :append "app")))
