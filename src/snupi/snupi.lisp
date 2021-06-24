(ql:quickload 'small)
(in-package :small)

;; Start the inferior shell, with input and output streams

;; Write a line to the shell
(progn
  (defparameter *shell* (uiop:launch-program "bash" :input :stream :output :stream))
  (write-line "cd /home/dd/PhD/dev/snupi/SNUPI_v1_01_linux/FILES/ && ./run_SNUPI.sh /usr/local/MATLAB/MATLAB_Runtime/v96"
              (uiop:process-info-input *shell*))
  ;; Flush stream
  (force-output (uiop:process-info-input *shell*))
  ;(read-line (uiop:process-info-output *shell*))


  (let ((stream (uiop:process-info-output *shell*)))
    (loop while (listen stream) do
      ;; Characters are immediately available
      (princ (read-line stream))
      (terpri)))
  (format t "AWE")
  ;(uiop:close-streams *shell*)
  (uiop:process-alive-p *shell*))



(defun run-snupi (run-dir)
  "run-dir: path to the file to folder which contains run_SNUPI.sh and Input.txt
Input.txt specifies the folder with the .json and .csv files of the design
In the directory .. to run-dir a directory with the name OUTPUT will be created, with subdirectories named DESIGN-DATE for each simulated design"
  (let ((cmd (format nil "cd ~A && ./run_SNUPI.sh /usr/local/MATLAB/MATLAB_Runtime/v96" run-dir)))
    (uiop:run-program cmd :force-shell t :output t)))


(run-snupi "/home/dd/PhD/dev/snupi/SNUPI_v1_01_linux/FILES/")
