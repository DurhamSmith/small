(defpackage :small
  (:use #:cl #:asdf #:magicl #:defclass-std)
  (:export #:v3       ; vector related 
	   #:x
	   #:y
	   #:z
	   #:print-v3 
	   #:chem-obj ; CHEM-OBJ related 
	   #:connect
	   #:dna      ; DNA CHEM-OBJ related
	   #:prev
	   #:next
	   #:dna-nt   ; DNA-NT CHEM-OBJ related
	   #:make-dna-nt
	   #:oxdna-config))

(in-package :small)
