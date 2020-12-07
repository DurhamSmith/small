(in-package :small)

(let* ((start-coord (v3 0 0 0))
       (vaxis (v3 1 0 0))
       (vbb0 (v3 0 1 0))
       (strand1 (SMALL::helix-strand start-coord vaxis vbb0 33))
       (strand2 (SMALL::helix-strand start-coord vaxis vbb0 33))
       (rot-mat (MAGICL:from-list  ;rotation matrix 90deg rotation around xy axis
	      `( 0d0 -1d0 0d0
		 1d0 0d0 0d0
		 0.0d0 0d0 1d0)
	      '(3 3)))
       (trans-vec (v3 0 5 0)) ; 5nm translation in y direction
       ss)
  (rotate-obj strand2 rot-mat)
  (translate-obj strand2 trans-vec)
  (setf ss (bridging-single-strand (cm (5nt strand1)) (cm (5nt strand2)) (v3 0 0 1)))
  (wmdna "ss-unconnected" (5nt strand1) (5nt ss) (5nt strand2))
;  (connect strand1 ss)
  (connect ss strand2)
  (wmdna "ss-connected" (5nt strand1) (5nt ss) (5nt strand2)))
  
