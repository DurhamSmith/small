(in-package :small)

;; Start the inferior shell, with input and output streams
(defparameter *shell* (uiop:launch-program "bash" :input :stream :output :stream))
;; Write a line to the shell
(write-line "cd /home/dd/PhD/dev/snupi/SNUPI_v1_01_linux/FILES/ && run_SNUPI.sh /usr/local/MATLAB/MATLAB_Runtime/v96"
            (uiop:process-info-input *shell*))
;; Flush stream
(force-output (uiop:process-info-input *shell*))
(read-line (uiop:process-info-output *shell*))

(let ((stream (uiop:process-info-output *shell*)))
     (loop while (listen stream) do
         ;; Characters are immediately available
         (princ (read-line stream))
         (terpri)))

* (uiop:close-streams *shell*)
* (uiop:process-alive-p *shell*)
