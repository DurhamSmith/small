(in-package #:small-tests)

(define-test "Test v3 creates a 3D vector with the correct type and shape"
  (let* ((x 1)
	 (y 2)
	 (z 3)
	 (type-spec '(double-float))
	 ((v (v3 x y z)))
	 (vx (tref v3 0))
	 (vy (tref v3 1))
	 (vz (tref v3 2)))
    (is = vx (coerce x type-spec))
    (is = vy (coerce y type-spec))
    (is = vz (coerce z type-spec))
    (is = (shape v) 3)))
