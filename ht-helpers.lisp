(in-package :small)

(defun +ht (ht v &key k)
  "Adds v to ht under key k. If no k, k=(concatenate 'string (type-of v) #vs-in-ht)"
  )

(defun numtype-in-ht (typ ht)
  "Returns the number of objects of type typ in ht"
  (length (mapcar #'(lambda (v)
		      (typep v typ))
		  (hash-table-values ht))))
  ;; (break "~A"  (length (maphash-values )
  ;; 				       ht))))
