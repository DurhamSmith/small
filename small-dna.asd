(asdf:defsystem small-dna
  :version "0.1.0"
  :description "An extensible to small for designing DNA-nanostructures."
  :license "Contact Authour"
  :author "Durham Smith <durham@futurefy.io>"
  :maintainer "Durham Smith <durham@futurefy.io>"
  ;;:homepage "https://durhamsmith.github.io/small/"
  :bug-tracker "https://github.com/DurhamSmith/small/issues"
  :source-control (:git "https://github.com/DurhamSmith/small.git")
  :serial T
  :components ((:file "src/dna/packages")
               (:file "src/dna/dna")
               (:file "src/dna/dna-nt")
               (:file "src/dna/dna-strand")
               (:file "src/dna/dna-helix-strand")
               (:file "src/dna/dna-single-strand")
               )
  :depends-on (:small))

;; (asdf:defsystem staple-dna
;;   :version "1.0.0"
;;   :license "zlib"
;;   :author "Nicolas Hafner <shinmera@tymoon.eu>"
;;   :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
;;   :description "Markdown processing support for Staple"
;;   :homepage "https://Shinmera.github.io/staple/"
;;   :bug-tracker "https://github.com/Shinmera/staple/issues"
;;   :source-control (:git "https://github.com/Shinmera/staple.git")
;;   :serial T
;;   :components ((:file "markdown"))
;;   :depends-on (:staple
;;                :3bmd
;;                :3bmd-ext-code-blocks))
