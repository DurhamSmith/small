;(ql:quickload :small)
(in-package :small)

(defparameter *oxview-path* "/home/user/software/oxview-electron/usr/bin/oxview")

(defun show-in-oxview (filename &rest dna)
  (write-as "oxdna" filename dna)
  (uiop:run-program (concatenate 'string *oxview-path* " " filename ".top " filename ".oxdna")))

(defun open-in-oxview (filename &key (conf-ext ".oxdna") (top-ext ".top") )
  (uiop:run-program (concatenate 'string *oxview-path* " " filename top-ext " " filename conf-ext)))

(defun oxview ()
  (uiop:run-program *oxview-path*))

(defun oxview-trajectory (topfile traj-file)
  (uiop:run-program (format nil "~A ~A ~A" *oxview-path* topfile traj-file)))
