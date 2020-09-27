(defpackage :small
  (:use #:cl #:asdf #:magicl #:defclass-std)
  (:export #:v3 #:x #:y #:z #:print-v3
	   #:make-dna-nt
	   #:dna-nt
	   #:oxdna-config))

(in-package :small)
