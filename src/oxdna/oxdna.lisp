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
  (sleep 2)
  ;; (let ((shell (uiop:launch-program "bash" :input :stream :output :stream)))
  ;;   (write-line (concatenate 'string
  ;;                           *oxdna-exe-path* " "
  ;;                            input-file)
  ;;               (uiop:process-info-input shell))
  ;;   (force-output (uiop:process-info-input shell))
  ;;   )
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
                        (format-oxdna-param (first key-val))
                        (format-oxdna-param (second key-val))))
            (group options-pairs 2)))
  filename)

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
"Handles case and formatting for writing oxDNA input files"
  (cond ((numberp param)
         param)
        ((stringp param)
         param)
        (t (string-downcase param))
        ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;    Data processing for plotting     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-data (input)
  "Reads in all data as lisp objects, returns list of read objects"
  (let* ((data (with-open-file (in input)
                (loop for line = (read in nil)
                      while line collect line))))
    data))

(defun data-range (data)
  "Returns the range of the data"
  (- (car (last data)) (first data)))

(defun sort-data (data)
  "Sorts data"
  (sort (remove-if-not #'floatp  data) #'>))



(defun data-between (data min max)
  "Takes data sorted with #'sort-data and returns how many data are in the interval [min,max)"
  (let* ((min-pos (position-if #'(lambda (d)
                                   (>= min d))
                               data))
         (max-pos (position-if #'(lambda (d)
                                   (> max d))
                               data))
         (num-in-bin (when min-pos
                                        ;(break "~A ~A ~A ~A" min-pos max-pos min max)
                       (if max-pos
                           (- max-pos min-pos)
                           (- (length data) min-pos)))))
    num-in-bin))


(defun bin-data (data &key (num-bins 5) (prob t))
  "Bins with [,) intervals. If prob=t then data is normalized to probability of data"
  (let* ((num-data (length data))
         (min (first data))
         (max (last data))
         (range (data-range data))
         (bin-width (/ range num-bins))
         (bins (loop for i from 1 upto num-bins collect
                                                (list (+ min (* bin-width (1- i)))
                                                      (data-between data
                                                                    (+ min (* bin-width (1- i) ))
                                                                    (+ min (* bin-width i)))))))
    (if prob
        (mapcar #'(lambda (bin)
                    (setf (second bin) (float (/ (second bin) num-data)))
                    bin)
                bins)
        bins)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Plotting              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun histogram-like-plot (data output &key title xlabel ylabel)
  "Plots histogram like data, data should be bined already with form ((interval_start num_entries) ..."
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
    (eazy-gnuplot:gp-setup :terminal '(pngcairo)
                           :output output
                           :title title
                           :xlabel xlabel
                           :ylabel ylabel)
    (eazy-gnuplot:plot
     (lambda ()
       (dolist (l data)
         (format t "~&~a ~a"  (car l) (second l))))
     :with '(:histeps)))
  output)

(defun histogram-plot (input output &key (num-bins 10) title xlabel ylabel)
  "Reads data in input where each line is x y val, sorts, bins and plots as histogram"
  (histogram-like-plot (bin-data (sort-data (get-data input)) :num-bins num-bins)
                       output
                       :title title
                       :xlabel xlabel
                       :ylabel ylabel))

(defun scatter-plot (input output &key title xlabel ylabel)
  "Writes data in input file (each line should be xval yval) to file named output"
  (eazy-gnuplot:with-plots (common-lisp:*standard-output* :debug nil)
    (eazy-gnuplot:gp-setup :terminal '(pngcairo)
                           :output output
                           :title title
                           :xlabel xlabel
                           :ylabel ylabel)
    (eazy-gnuplot:plot
     (pathname input)
      :with '(:histeps)
     ))
  output)
