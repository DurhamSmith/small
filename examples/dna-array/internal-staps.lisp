(ql:quickload :small)
(in-package :small)

(load "./part1.lisp")

(load "./part2.lisp")

(setq tri (make-instance 'dna-triangle))

(wmdna "./tri"  (5nt (first (scaffold tri))))

(progn





(defun alternate (a b len)
  (loop for x from 1 to len collect
        (if (oddp x)
            a
            b)))

(defun staple-ordered-antiparallel-strands (strands i starts lengths &key from-3end desc)
  "Creates a staple strand to hold strands from together.
strands: List DNA strands that have property :i with value some numerical index. Subsequent indices should be antiparrallel alligned
i: First indice, subsequent are i+1 up to i+(length (-1 ) starts)
starts: list of nt offset
lengths: list how long that section of the staple should be
from-3end: if the first (:i i) start should be from the 5 end or 3 end
desc: indices traveresd i-..."
  (let* ((helices (if desc
                      (loop for x from i above (- i (length starts))
                            collect
                            (SMALL::find-obj-with-props (scaffold tri)
                                                        `((:i . ,x))))
                      (loop for x from i below (+ i (length starts))
                            collect
                            (SMALL::find-obj-with-props (scaffold tri)
                                                        `((:i . ,x))))))
         (ends (mapcar #'+ starts lengths))
         (f3ends (alternate (if from-3end
                                t
                                nil)
                            (if from-3end
                                nil
                                t)
                            (length starts)))
         (staple-spec (mapcar #'(lambda (hel start end f3e)
                                  (list :obj hel :start start :end end :from-3end f3e))
                              helices starts ends f3ends)))
    staple-spec))

(wmdna "./tmp/tri-is-r1" (list
                (5nt (first (scaffold tri)))
                (loop for i from 22 downto 4 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(15 15 22) '(8 15 8)  ;; zero based index
                        :desc t
                        :from-3end nil)))
                ))

(wmdna "./tmp/tri-is-r2" (list
                (5nt (first (scaffold tri)))
                (loop for i from 5 upto 17 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(38 30 30) '(8 16 8)
                        :desc nil
                        :from-3end t)))
                ))

(wmdna "./tmp/tri-is-r3" (list
                (5nt (first (scaffold tri)))
                (loop for i from 18 downto 8 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(46 46 54) '(8 16 8)
                        :desc t
                        :from-3end nil)))))


(wmdna "./tmp/tri-is-r4" (list
                (5nt (first (scaffold tri)))
                (loop for i from 7 upto 15 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(69 62 62) '(8 15 7)
                        :desc nil
                        :from-3end t)))
                ))

(wmdna "./tmp/tri-is-r5" (list
                (5nt (first (scaffold tri)))
                (loop for i from 14 downto 12 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(77 77 85) '(8 16 8)
                        :desc t
                        :from-3end nil)))))

(wmdna "./tmp/tri-is-r6"
       (5nt (first (scaffold tri)))
       (create-staple
        (staple-ordered-antiparallel-strands
         (scaffold tri)
         11
         '(101 93 93) '(8 16 8)  ;; zero based index
         :desc nil
         :from-3end t))
       )

(wmdna "./tmp/all" (list
                (5nt (first (scaffold tri)))
                (loop for i from 14 downto 12 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(77 77 85) '(8 16 8)
                        :desc t
                        :from-3end nil)))
                (loop for i from 7 upto 15 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                        '(69 62 62) '(8 15 7)
                        :desc nil
                        :from-3end t)))
                (loop for i from 18 downto 8 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(46 46 54) '(8 16 8)
                        :desc t
                        :from-3end nil)))
                (loop for i from 5 upto 17 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(38 30 30) '(8 16 8)
                        :desc nil
                        :from-3end t)))
                (loop for i from 22 downto 4 by 2
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(15 15 22) '(8 15 8)  ;; zero based index
                        :desc t
                        :from-3end nil)))
                (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        11
                       '(101 93 93) '(8 16 8)  ;; zero based index
                        :desc nil
                        :from-3end t))
                ))


(create-staple
 (staple-ordered-antiparallel-strands
  (scaffold tri)
  22
  '(24 17 24) '(8 15 8)
  :from-3end t
  :desc t))
)
