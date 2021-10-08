(in-package :small)

(setf ext-dna (make-instance 'extension
		    :files '("dna.lisp" "dna-nt.lisp" "dna-strand.lisp" "dna-helix-strand.lisp" "dna-single-strand.lisp" "dna-origami.lisp")
		    :uuid 18
		    :name "DNA Model"
		    ;':doi "4.4.4.4"
		    :dir "/home/dd/quicklisp/local-projects/small/src/dna"
		    :desc "A model for DNA. Provides models for nucleotides (DNA-NT), multiple types of strands (DNA-HELIX-STRAND, DNA-SINGLE-STRAND, DNA-STAPLE-STRAND) and origami (DNA-ORIGAMI). Also provides functions for common tasks such as creating staple strands, finding crossover points and writing to various file formats."
		    ))

(upload-design ext-dna)
*credentials*

(setf ext-np (make-instance 'extension
                             :files '("nanoparticle.lisp")
                             :uuid 2
                             :name "Nanoparticle Model"
                                        ;':doi "4.4.4.4"
                             :dir "/home/dd/quicklisp/local-projects/small/src/nanoparticle"
                             :desc "A model for cubic nanoparticle"
                             ))

(upload-design ext-np)

(setf ext-oxdna (make-instance 'extension
                             :files '("oxdna.lisp")
                             :uuid 3
                             :name "oxDNA Integration"
                                        ;':doi "4.4.4.4"
                             :dir "/home/dd/quicklisp/local-projects/small/src/oxdna"
                             :desc "Integration with the oxDNA molecular dynamics simulator"
                             ))

(upload-design ext-oxdna)


(setf ext-snupi (make-instance 'extension
                             :files '("snupi.lisp")
                             :uuid 4
                             :name "SNUPI (Structured NUcleic acids Programming Interface) Integration"
                                        ;':doi "4.4.4.4"
                             :dir "/home/dd/quicklisp/local-projects/small/src/snupi"
                             :desc "Integration with the SNUPI (Structured NUcleic acids Programming Interface) Multiscale Analysis Framework"
                             ))

(upload-design ext-snupi)



(setf ext-meep (make-instance 'extension
                             :files '("meep.lisp")
                             :uuid 5
                             :name "Meep and MPB Integration"
                                        ;':doi "4.4.4.4"
                             :dir "/home/dd/quicklisp/local-projects/small/src/meep"
                             :desc "Integration with MIT Electromagnetic Equation Propagation (Meep) and MIT Photonic Bands (MPB) software packages"
                             ))

(upload-design ext-meep)

(setf ext-vmd (make-instance 'extension
                             :files '("vmd.lisp")
                             :uuid 6
                             :name "Visual Molecular Dynamics (VMD) Integration"
                                        ;':doi "4.4.4.4"
                             :dir "/home/dd/quicklisp/local-projects/small/src/vmd"
                             :desc "Integration with the Visual Molecular Dynamics software package"
                             ))

(upload-design ext-vmd)
