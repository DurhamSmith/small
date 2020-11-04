(in-package :small)
(defparameter *c* (make-instance 'dna-cube))


(break *c*)
(wmdna "ct"
       (first (all-to-write (c1 *c*)))
       ;(joining-strands (t1 (c1 *c*)))
       (first (all-to-write (t2 (c1 *c*))))
       ;(joining-strands (t2 (c1 *c*)))
       (first (all-to-write (t3 (c1 *c*))))
       ;(joining-strands (t3 (c1 *c*)))

       (first (all-to-write (c2 *c*)))
       (joining-strands (t1 (c2 *c*)))
       (first (all-to-write (t2 (c2 *c*))))
       ;(joining-strands (t2 (c2 *c*)))
       (first (all-to-write (t3 (c2 *c*))))
					;(joining-strands (t3 (c2 *c*)))
       )

(wmdna "ct" (3nt (first (all-to-write (c1 *c*))))
       (5nt (first (all-to-write (c2 *c*)))))

(joining-strands (t1 (c1 *c*)))
