(asdf:defsystem #:small
  :description "A common-lisp nanosystem design tool."
  :version "0.0.1"
  :author "Durham Smith"
  :licence "Contact author"
  :depends-on (#:magicl #:defclass-std)
  :serial t
  :components ((:file "packages")
	       (:file "linear-algebra")
	       (:file "chem-obj")
	       ;; (:file "dna" :depends-on ("chem-obj"))
	       ;; (:file "dna-nt" :depends-on ("dna"))
	       ;; (:file "dna-strand" :depends-on ("dna-nt"))
	       ;; (:file "dna-single-strand" :depends-on ("dna-strand"))
	       ;; (:file "dna-helix-strand" :depends-on ("dna-strand"))
	       ;; (:file "dna-tile" :depends-on ("dna-single-strand" "dna-helix-strand"))
	       )
  :in-order-to ((asdf:test-op (asdf:test-op #:small-tests))))

  

(asdf:defsystem #:small-tests
  :description "Regression tests for small."
  :version "0.0.1"
  :author "Durham Smith"
  :licence "Contact author"
  :depends-on (#:small
	       #:parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :small-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "chem-obj-tests")
	       ;; (:file "dna-tests")
	       ;; (:file "dna-tile-tests")
	       ))

