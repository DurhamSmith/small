;(ql:quickload :small)
(in-package :small)

(defun open-in-vmd (&rest files)
  (let ((shell (uiop:launch-program "bash" :input :stream :output :stream)))
  (write-line (concatenate 'string
                                 "vmd "
                                 (format nil "~{~a~^ ~}" files))
              (uiop:process-info-input shell))
  (force-output (uiop:process-info-input shell))))




(defun show-in-vmd (filename &rest dna)
  (write-as "oxdna" filename dna)
  (tacox-convert filename :from 'oxdna :to 'pdb)
  (uiop:rename-file-overwriting-target
   (concatenate 'string filename ".oxdna.pdb")
   (concatenate 'string filename ".pdb"))
   (open-in-vmd (concatenate 'string filename ".pdb")))
