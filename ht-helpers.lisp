(in-package :small)

(defun +ht (ht v &key k)
  "Adds v to ht under key k. If no k, k=(concatenate 'string (type-of v) #vs-in-ht)"
  (let* ((k (if k
		k
		(concatenate 'string
			 (clsnm-as-str v)
			 " "
			 (format nil "~A"
				 (numtype-in-ht (type-of v) ht))))))
    (setf (gethash k ht) v)))

(defun numtype-in-ht (typ ht)
  "Returns the number of objects of type typ in ht"
  (let* ((l (length (mapcar #'(lambda (v)
		      (typep v typ))
			   (hash-table-values ht)))))
    (if l
	l
	0)))
	
	
  ;; (break "~A"  (length (maphash-values )
  ;; 				       ht))))


(defun clsnm-as-str (x)
    "Returns x's class-name as a sting"
  ;;untested
  (string (class-name (class-of x))))
