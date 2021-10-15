(defun run-oxdna (input-file)
  "runs an oxdna sim"
  (let ((shell (uiop:launch-program "bash" :input :stream :output :stream)))
    (write-line (concatenate 'string
                             "oxDNA "
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
