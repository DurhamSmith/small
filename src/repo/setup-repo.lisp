(in-package :small)

(setf ext-dna (make-instance 'extension
		    :files '("dna.lisp" "dna-nt.lisp" "dna-strand.lisp" "dna-helix-strand.lisp" "dna-single-strand.lisp" "dna-origami.lisp")
		    :uuid 1
		    :name "DNA Model"
		    ;':doi "4.4.4.4"
		    :dir "/home/dd/quicklisp/local-projects/small/src/dna"
		    :desc "A model for DNA. Provides models for nucleotides (DNA-NT), multiple types of strands (DNA-HELIX-STRAND, DNA-SINGLE-STRAND, DNA-STAPLE-STRAND) and origami (DNA-ORIGAMI). Also provides functions for common tasks such as creating staple strands, finding crossover points and writing to various file formats."
		    ))

(upload-design ext-dna)
