(in-package :small)
(load "../part1.lis p")

(load "../part2.lisp")









(let ((tri (make-instance 'dna-triangle)))
  (wmdna "./tri_int"
         (5nt (first (scaffold tri)))
         (staples tri)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 CORNER                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        staple bridges to corner       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;










(let ((corner (make-instance 'dna-corner)))
  (wmdna "corner_stap_briges_internal_staps_1"
         (car (all-corner corner))
         (staple-bridges-corner corner)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Triangle Joining          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
















(let* ((c (make-instance 'dna-cube))
       ;(t1 (t1 c))
       ;(t2 (t2 c))
       ;; (staps (join-triangle t1 t2))
       ;; (staps (join-cube c))
       )
  ;staps
  (push (capping-ends (t1 (c1 c))) (staples (t1 (c1 c))))
  (wmdna "joining"
         (all-triangle (t1 (c1 c)))
         ;staps
         ))
