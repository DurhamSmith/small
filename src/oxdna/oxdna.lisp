(defparameter *oxdna-path* "/home/dd/PhD/Software/oxDNA/")
(defparameter *oxdna-exe-path* "/home/dd/PhD/Software/oxDNA/build/bin/oxDNA")
(defparameter *oxdna-utils-path* "/home/dd/PhD/Software/oxDNA/UTILS/")



(defun set-oxdna-path (oxdna-path)
  (setf *oxdna-path* oxdna-path
        *oxdna-exe-path* (concatenate 'string oxdna-path "/bulid/bin/oxDNA")
        *oxdna-utils-path* (concatenate 'string oxdna-path "/UTILS/")))


(defun run-oxdna (input-file)
  "runs an oxdna sim"
  (let ((shell (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line (concatenate 'string
                            *oxdna-exe-path* " "
                             input-file)
                (uiop:process-info-input shell))
    (force-output (uiop:process-info-input shell))))


(defmacro make-oxdna-input-file (filename &rest options-pairs)
  ;(break "~A" (group options-pairs 2))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (mapcar #'(lambda (key-val)
;                (break "~A" key-val)
                (format stream "~A  = ~A ~%" (first key-val) (second key-val)))
            (group options-pairs 2)))
  filename)

(run-oxdna-util output_bonds.py <input_file> <trajectory_file> [counter])

(defmacro run-oxdna-util (&rest rest)
  (let ((command (concatenate 'string
                              *oxdna-utils-path*
                              (format nil "~{ ~a~}" rest)))
        (shell (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line command
                (uiop:process-info-input shell))
    (force-output (uiop:process-info-input shell))))
