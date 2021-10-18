(defparameter *oxdna-path* "/home/dd/PhD/Software/oxDNA/")
(defparameter *oxdna-exe-path* "/home/dd/PhD/Software/oxDNA/build/bin/oxDNA")
(defparameter *oxdna-utils-path* "/home/dd/PhD/Software/oxDNA/UTILS/")



(defun set-oxdna-path (oxdna-path)
  (setf *oxdna-path* oxdna-path
        *oxdna-exe-path* (concatenate 'string oxdna-path "/bulid/bin/oxDNA")
        *oxdna-utils-path* (concatenate 'string oxdna-path "/UTILS/")))


(defun run-oxdna (input-file)
  "runs an oxdna sim"
  (break "oxDNA simulation starting on input file ~A" input-file)
  (let ((shell (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line (concatenate 'string
                            *oxdna-exe-path* " "
                             input-file)
                (uiop:process-info-input shell))
    (force-output (uiop:process-info-input shell))
    )
  (break "oxDNA simulation on input file ~A complete" input-file))


(defmacro make-oxdna-input-file (filename &rest options-pairs)
  "Writes options-pairs to filename. options-pairs should have the form of a key followed by a val for every option. Make sure options-pairs is case correct. Valid options-pairs arguments can be found at https://dna.physics.ox.ac.uk/index.php/Input_options"
  ;(break "~A" (equalp 'sim_type (car options-pairs)))
  ;(ca)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (mapcar #'(lambda (key-val)
;                (break "~A" key-val)
                (format stream "~A  = ~A ~%"
                        ;; (break "~A ~A" (first key-val)
                        ;; (second key-val))
                        (string-downcase (first key-val))
                        (second key-val)))
            (group options-pairs 2)))
  filename)
(string 2)
;(run-oxdna-util output_bonds.py <input_file> <trajectory_file> [counter])

(defmacro run-oxdna-util (&rest rest)
  "Runs the util program. It is launched as if all rest args are passed to the command line verbatium"
  (let ((command (concatenate 'string
                              *oxdna-utils-path*
                              (format nil "~{ ~a~}" rest)))
        (shell (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line command
                (uiop:process-info-input shell))
    (force-output (uiop:process-info-input shell))))


(defun format-oxdna-param (param)


  (cond ((equalp 'sim_type param)
         (string-upcase param))
        ((equalp 'backend param)
         (string-upcase param))
        ((equalp 'backend_precision param)
         (string-upcase param))
         ))))
