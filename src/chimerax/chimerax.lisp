;(ql:quickload :small)
(in-package :small)

(defun run-chimerax (filename  &rest dna)
  (write-as "oxdna" filename dna)
  (tacox-convert filename :from 'oxdna :to 'pdb)
  (uiop:rename-file-overwriting-target
   (concatenate 'string filename ".oxdna.pdb")
   (concatenate 'string filename ".pdb")))


  ;(uiop:run-program (concatenate 'string "oxview " filename ".top " filename ".oxdna")))


(run-chimerax "atest" (helix-strand (v3 1 0 0) (v3 0 1 0) (v3 0 0 1) 10))
