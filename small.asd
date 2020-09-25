(asdf:defsystem #:small
:description "A common-lisp nanosystem design tool."
:version "0.0.1"
:author "Durham Smith"
:licence "Contact author"
:depends-on (#:magicl #:prove)
  :components ((:file "packages")
	       (:file "linear-algebra")
	       (:file "chem-obj")
	       (:file "dna" :depends-on ("chem-obj"))
	       (:file "dna-nt" :depends-on ("chem-obj"))
	       (:file "dna-strand" :depends-on ("dna-nt"))
	       (:file "dna-single-strand" :depends-on ("dna-strand"))
	       (:file "dna-helix-strand" :depends-on ("dna-strand"))
	       (:file "dna-tile" :depends-on ("dna-single-strand" "dna-helix-strand"))))
