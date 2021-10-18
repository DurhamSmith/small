;(ql:quickload :small)
(in-package :small)


(defun show-in-oxview (filename &rest dna)
  (write-as "oxdna" filename dna)
  (uiop:run-program (concatenate 'string "oxview " filename ".top " filename ".oxdna")))

(defun open-in-oxview (filename &key (conf-ext ".oxdna") (top-ext ".top") )
  (uiop:run-program (concatenate 'string "oxview " filename top-ext " " filename conf-ext)))

(defun oxview ()
  (uiop:run-program "oxview"))

(defun oxview-trajectory (topfile traj-file)
  (uiop:run-program (format nil "oxview ~A ~A" topfile traj-file)))
