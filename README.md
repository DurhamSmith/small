* Adding of parents/children
* Writing of multiple DNA objects or one object with many children
* Storing of COs that combine N conceptual objects, N > 1, e.g. linking sub-origami structures.
** Moving objects and their subobjects
*** Creation of new (sub) objs
 E.g.maving positioned to sub-origami elements and creating a parental container to hold them if staples need to be added they need to be added from the view of things being rotated/translated first (to bridge new coords). But if we are stitching something together that is 
*** Positioning of structural children on creation 
*** ? Joining sub objs on creation
* Model for single strand linkers (and ones in close proximity)





* In field

** Make partner when there are already some ocupied nts


* TODO Check that when making staple we make them in the correct 5->3 

* While Writing Article
\textcolor{red}{Should connect be at the chem-obj level or dna level?}
** For cearting partners for DNA
\textcolor{red}{Should I also make a destructive keyword?}


 \textcolor{red}{If the destructive keyword is given as \lstinline{t} then \lstinline{partner} class slot of the argument \lstinline{dna} object is set to the created \lstinline{dna} object partner and similarly the \lstinline{dna} object partner is set to the argument \lstinline{dna} object.}
 
 ** DNA-NT section notes
The \lstinline{dna-nt} class adds implementations \textcolor{red}{(specializations?)} the generic functions \lstinline{connect} \lstinline{make-partner} 
 and \textcolor{red}{TODO: update code so nts replaces connected-nts}. \textcolor{red}{TODO Check what to do about the helper-like functions that we implementation in dna-nt.lisp. }

\textcolor{red}{TODO: Should I add the vbb vn and vcm rotation and transformations information to capture why we have the chem-obj class?}

