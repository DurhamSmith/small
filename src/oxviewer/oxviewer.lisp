(ql:quickload :small)
(in-package :small)




;;(show-in-oxview "cb" (all-cube (make-instance 'dna-cube)))

(defun show-in-oxview (filename &rest dna)
  (write-as "oxdna" filename dna)
  (uiop:run-program (concatenate 'string "oxview " filename ".top " filename ".oxdna")))
