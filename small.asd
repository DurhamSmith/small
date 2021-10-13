(asdf:defsystem small
  :version "0.1.0"
  :description "An extensible nanosystem design tool."
  :license "Contact Authour"
  :author "Durham Smith <durham@futurefy.io>"
  :maintainer "Durham Smith <durham@futurefy.io>"
  ;;:homepage "https://durhamsmith.github.io/small/"
  :bug-tracker "https://github.com/DurhamSmith/small/issues"
  :source-control (:git "https://github.com/DurhamSmith/small.git")
  :serial T
  :depends-on (#:alexandria #:magicl #:defclass-std #:fset)
  :components ((:file "packages")
               (:file "src/core/linear-algebra")
               (:file "src/core/chem-obj")
               (:file "src/core/utils")
               (:file "src/core/ht-helpers")
               (:file "src/dna/dna")
               (:file "src/dna/dna-nt")
               (:file "src/dna/dna-strand")
               (:file "src/dna/dna-helix-strand")
               (:file "src/dna/dna-single-strand")
               (:file "src/dna/dna-origami")
               (:file "src/tacoxdna/tacoxdna")
               (:file "src/oxviewer/oxviewer")
               (:file "src/chimerax/chimerax")
               (:file "src/vmd/vmd")
               ))

;;(make-instance 'dna-nt )

;; (asdf:defsystem small-dna
;;   :version "0.1.0"
;;   :description "An extensible to small for designing DNA-nanostructures."
;;   :license "Contact Authour"
;;   :author "Durham Smith <durham@futurefy.io>"
;;   :maintainer "Durham Smith <durham@futurefy.io>"
;;   ;;:homepage "https://durhamsmith.github.io/small/"
;;   :bug-tracker "https://github.com/DurhamSmith/small/issues"
;;   :source-control (:git "https://github.com/DurhamSmith/small.git")
;;   :serial T
;;   :components ((:file "src/dna/packages")
;;                (:file "src/dna/dna")
;;                (:file "src/dna/dna-nt")
;;                (:file "src/dna/dna-strand")
;;                (:file "src/dna/dna-helix-strand")
;;                (:file "src/dna/dna-single-strand")
;;                )
;;   :depends-on (:small))


;; (asdf:defsystem #:small
;;   :description "A common-lisp nanosystem design tool."
;;   :version "0.0.1"
;;   :author "Durham Smith"
;;   :licence "Contact author"
;;   :depends-on (#:alexandria #:magicl #:defclass-std #:fset)
;;   :serial t
;;   :components ((:file "packages")
;; 	       (:file "src/core/linear-algebra")
;; 	       (:file "ht-helpers")
;; 	       (:file "chem-obj")
;; 	       (:file "dna" :depends-on ("chem-obj"))
;; 	       (:file "dna-nt" :depends-on ("dna"))
;; 	       (:file "dna-strand" :depends-on ("dna-nt"))
;; 	       (:file "dna-single-strand" :depends-on ("dna-strand"))
;; 	       (:file "dna-helix-strand" :depends-on ("dna-strand"))
;; 	       (:file "dna-origami" :depends-on ("dna-single-strand" "dna-helix-strand"))
;; 	       (:file "dna-tile" :depends-on ("dna-single-strand" "dna-helix-strand"))
;; 	       (:file "dna-triangle" :depends-on ("dna-origami"))
;; 	       (:file "dna-cone" :depends-on ("dna-triangle"))
;; 	       (:file "dna-cube" :depends-on ("dna-cone"))
;; 	       (:file "utils" :depends-on ("dna-cube"))
;; 	       )
;;   :in-order-to ((asdf:test-op (asdf:test-op #:small-tests))))



;; (asdf:defsystem #:small-tests
;;   :description "Regression tests for small."
;;   :version "0.0.1"
;;   :author "Durham Smith"
;;   :licence "Contact author"
;;   :depends-on (#:small
;; 	       #:parachute)
;;   :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :small-tests))
;;   :pathname "tests/"
;;   :serial t
;;   :components ((:file "package")
;; 	       (:file "tile-data")
;; 	       (:file "linear-algebra-tests")
;; 	       (:file "ht-helpers-tests")
;;                (:file "chem-obj-tests")
;; 	       (:file "dna-tests")
;; 	       (:file "dna-nt-tests")
;; 	       (:file "dna-strand-tests")
;; 	       (:file "dna-helix-strand-tests")
;; 	       (:file "dna-single-strand-tests")
;; 	       ;; (:file "dna-tile-tests")
;; 	       (:file "dna-origami-tests")
;; 	       (:file "util-tests")
;; 	       ))
