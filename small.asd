(asdf:defsystem #:small
  :description "A common-lisp nanosystem design tool."
  :version "0.0.1"
  :author "Durham Smith"
  :licence "Contact author"
  :depends-on (#:alexandria #:magicl #:defclass-std)
  :serial t
  :components ((:file "packages")
	       (:file "linear-algebra")
	       (:file "ht-helpers")
	       (:file "chem-obj")
	       (:file "dna" :depends-on ("chem-obj"))
	       (:file "dna-nt" :depends-on ("dna"))
	       (:file "dna-strand" :depends-on ("dna-nt"))
	       (:file "dna-single-strand" :depends-on ("dna-strand"))
	       (:file "dna-helix-strand" :depends-on ("dna-strand"))
	       (:file "dna-origami" :depends-on ("dna-single-strand" "dna-helix-strand"))
	       (:file "dna-tile" :depends-on ("dna-single-strand" "dna-helix-strand"))
	       (:file "dna-triangle" :depends-on ("dna-origami"))
	       (:file "dna-cone" :depends-on ("dna-triangle"))
	       (:file "dna-cube" :depends-on ("dna-cone"))
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
	       (:file "tile-data")
	       (:file "linear-algebra-tests")
	       (:file "ht-helpers-tests")
               (:file "chem-obj-tests")
	       (:file "dna-tests")
	       (:file "dna-nt-tests")
	       (:file "dna-strand-tests")
	       (:file "dna-helix-strand-tests")
	       (:file "dna-single-strand-tests")
	       ;; (:file "dna-tile-tests")
	       (:file "dna-origami-tests")
	       ))

