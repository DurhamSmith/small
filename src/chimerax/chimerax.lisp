;(ql:quickload :small)
(in-package :small)

(defun open-in-chimerax (&rest files)
  (uiop:run-program (concatenate 'string
                                 "chimerax "
                                 (format nil "~{~a~^ ~}" files))))


(defun show-in-chimerax (filename &rest dna)
  (write-as "oxdna" filename dna)
  (tacox-convert filename :from 'oxdna :to 'pdb)
  (uiop:rename-file-overwriting-target
   (concatenate 'string filename ".oxdna.pdb")
   (concatenate 'string filename ".pdb"))
   (open-in-chimerax (concatenate 'string filename ".pdb")))
