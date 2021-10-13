;(ql:quickload :small)
(in-package :small)


(defparameter *tacoxdna-path* "/home/dd/PhD/Software/tacoxDNA")
(defun tacox-convert (filename &key from to)
  ;;(funcall (find-conversion from to))
  (uiop:run-program (concatenate 'string
                                 "python "
                                 *tacoxdna-path* "/src/oxDNA_PDB.py "
                                 filename ".top " filename ".oxdna 53 -H true")))
