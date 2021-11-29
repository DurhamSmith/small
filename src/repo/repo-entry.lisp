(ql:quickload 'small)
(in-package :small)
(ql:quickload 'zs3)
(ql:quickload 'yason)
(ql:quickload 'defclass-std)
;(defpackage #:small-repo (:use #:cl #:zs3 #:defclass-std))
(use-package '(:zs3 :yason :defclass-std))

(defparameter *bucket-name* "zs3-demo")

(defclass environment-credentials () ())

(defmethod access-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (getenv "***REMOVED***"))

(defmethod secret-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (getenv "***REMOVED***"))

(setf *credentials* (make-instance 'environment-credentials))



(setf *credentials* (zs3:file-credentials "~/.aws/zs3-creds"))

;; (bucket-exists-p "zs3-demo")

;; (create-bucket "zs3-demo"))

;; (put-vector (octet-vector 8 6 7 5 3 0 9 ) "zs3-demo" "jenny")

;; (copy-object :from-bucket "zs3-demo" :from-key "jenny" :to-key "j2")

;; (get-vector "zs3-demo" "j2")

;; (put-file "~/quicklisp/local-projects/small/small-repo/repo-entry.lisp" "zs3-demo" "t/3")


(defclass/std extension ()
  ((files :doc "List of files, in the order they should be loaded")
   (dir :doc "The directory where files should be written to")
   (name :doc "Name of the design")
   (uuid :doc "unique id of the design")
   (doi :doc "DOI relating to papers that use the design")
   (desc :doc "Description of the design")))

;; (get-file "zs3-demo"  "repo-entry.lisp" "~/quicklisp/local-projects/small/small-repo/repo-entry2.lisp")

;; (get-file "zs3-demo"  "jenny" "~/quicklisp/local-projects/small/small-repo/jenny.txt")
;; (directory "~/")
;; (uiop:directory* "~/")
;; (all-keys "zs3-demo")
;; (setq *response* (query-bucket "zs3-demo" :delimiter "/" :prefix "t/"))
;; (values (keys *response*) (common-prefixes *response*))

;; (get-file "zs3-demo" (name (aref (keys *response*) 0))
;; 	  (format nil "/home/dd/quicklisp/local-projects/small/small-repo/~A"
;; 		  (name (aref (keys *response*) 0))))


(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (with-output-to-string (s)
        (dolist (item list)
          (if (stringp item)
              (format s "~a" item))))
      list))

;; (format nil "(~{~a~^ ~})" '("dna.lisp" "dna-nt.lisp" "dna-strand.lisp" "dna-single-strand.lisp" "dna-helix-strand.lisp" "dna-origami.lisp"))

;; (format nil "(~{~a~^ ~})" 1)

;; (format nil "~A" (files ext-dna))
;; (concatString (files ext-dna))
;; (design-info-list ext-dna)
;; (design-info-string ext-dna)
;; (design-info-list d)

(defun process-slot-val (sv)
  (if (listp sv)
      (format nil "(~{~a~^ ~})" sv)
      sv))

(defun design-info-list (d)
  "Generates a desciption of the design to be stored on uuid/info"
  (mapcar #'(lambda (slot)
	    (format nil "~A: ~A~%"
		    slot
            (process-slot-val (funcall slot d))))
	  (remove 'DIR
		  (mapcar #'sb-mop:slot-definition-name
			  (sb-mop:class-direct-slots (class-of d))))))

(defun design-info-string (d)
  (concatstring (design-info-list d)))

;; (design-info-list ext-dna)
;; (design-info-string ext-dna)

;; (setf d (make-instance 'extension
;; 		    :files '("part1.lisp" "part2.lisp")
;; 		    :uuid 6912
;; 		    :name "Test"
;; 		    :doi "4.4.4.4"
;; 		    :dir "/home/dd/quicklisp/local-projects/small/src/repo/tst"
;; 		    :desc "DNA tile implementation that i want show mike"
;; 		    ))

;; (upload-design d)


(defun upload-design (des)
  (mapcar #'(lambda (f)
	      (let ((dest-path (format nil "~A/~A" (uuid des) f))
		    (src-path (format nil "~A/~A" (dir des) f)))			   
		(ZS3:put-file src-path
			  *bucket-name*
			  dest-path)))
	  (files des))
  (ZS3:put-string (design-info-string des)
	      *bucket-name*
	      (format nil "~A/info" (uuid des))))



;;(upload-design ext-dna)
(defun load-extension (uud)
  (format nil "Extension 7: DNA-ARRAY loaded"))
;; (defun download-design (des)
;;   (map 'list
;;        #'(lambda (key)
;;            (zs3::get-file *bucket-name*
;;                           key
;;                           (format
;;                            nil
;;                            "/home/dd/quicklisp/local-projects/small/small-repo/down/~A"
;;                            (name key))))
;; 	   (keys (zs3::query-bucket "zs3-demo"
;;                                 :delimiter "/"
;;                                 :prefix (format nil "~A/" (uuid des))))))
       
;; (download-design d)
;; (map 'list (keys (download-design d)))


;; (let ((a (ZS3:get-string "zs3-demo" "1/info")))
;;   (format nil "~& ~A ~%" a))


;; (design-info-string ext-dna)

;; {
;; "FILES": "(part1.lisp part2.lisp)",
;; "NAME": "Test",
;; "UUID": "1",
;; "DOI": "4.4.4.4",
;; "DESC": "DNA tile implementation"
;; }
