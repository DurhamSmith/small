;(ql:quickload :small)
(in-package :small)


(defun show-in-oxview (filename &rest dna)
  (write-as "oxdna" filename dna)
  (uiop:run-program (concatenate 'string "oxview " filename ".top " filename ".oxdna")))
