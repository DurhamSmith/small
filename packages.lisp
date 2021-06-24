(defpackage :small
  (:use #:cl #:alexandria #:asdf #:magicl #:defclass-std #:fset-user)
  (:shadowing-import-from :common-lisp "SOME" "=" "MAP" "TRACE" "NOTANY" "NOTEVERY" "EVERY")
  (:export #:v3       ; vector related 
	   #:v3l
	   #:x
	   #:y
	   #:z
	   #:as-unit-vec
	   #:print-v3
	   #:on-v3-axis
	   #:bounds
	   #:rotation-matrix
	   #:rotate-vec
	   #:cylindrical->cartesian-matrix
	   #:cylindrical->cartesian
	   #:write-list ; MISC
	   #:+ht ;hash table stuff
	   #:numtype-in-ht
	   #:oxdna->fileb
	   #:chem-obj ; CHEM-OBJ related
	   #:set-parent
	   #:set-child
	   #:connect
	   #:dna      ; DNA CHEM-OBJ related
	   #:dna-connect
      	   #:prev
	   #:next
	   #:5end
	   #:3end
	   #:dna-nt   ; DNA-NT CHEM-OBJ related
	   #:base
	   #:make-dna-nt
	   #:connect-nts
	   #:connected-nts
	   #:oxdna-config
	   #:oxdna-config-string
	   #:oxdna-config-header
	   #:oxdna-box-size
	   #:oxdna-topology
	   #:oxdna-topology-from-seq
	   #:write-oxdna
	   #:make-dna-strand   ;dna-strand
	   #:add-nt
	   #:dna-single-strand
	   #:dna-helix-strand
	   #:t2))

;(in-package :small)
