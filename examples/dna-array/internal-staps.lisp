(ql:quickload :small)
(in-package :small)

(load "./part1.lisp")

(load "./part2.lisp")

(setq tri (make-instance 'dna-triangle))

(wmdna "./tri"  (5nt (first (scaffold tri))))



(defun s-staple (tile k i starts lengths)
  "creates an s-shaped staple strand to hold tile helices i, i+1 and i+2 together.
Starts are taken from tile edges"
  (let* ((hi (SMALL::find-obj-with-props (scaffold tile)
					  `((:i . ,i) (:k . ,k))))
	 (hi+1 (small::find-obj-with-props (scaffold tile)
					  `((:i . ,(+ i 1)) (:k . ,k))))
	 (hi+2 (small::find-obj-with-props (scaffold tile)
					   `((:i . ,(+ i 2)) (:k . ,k))))
	 (ends (mapcar #'+ starts lengths)))
    (if (evenp i) ;;TODO: Add error checking on i and k
	(SMALL::create-staple `((:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end nil)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end t)
				(:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end nil)))
	(SMALL::create-staple `((:obj ,hi  :start ,(first starts) :end ,(first ends) :from-3end t)
			        (:obj ,hi+1  :start ,(second starts) :end ,(second ends) :from-3end nil)
				(:obj ,hi+2  :start ,(third starts) :end ,(third ends) :from-3end t))))))



(defun alternate (a b len)
  (loop for x from 1 to len collect
        (if (oddp x)
            a
            b)))

(defun staple-ordered-antiparallel-strands (strands i starts lengths &key from-3end down)
  "Creates a staple strand to hold strands from together.
strands: List DNA strands that have property :i with value some numerical index. Subsequent indices should be antiparrallel alligned
i: First indice, subsequent are i+1 up to i+(length (-1 ) starts)
starts: list of nt offset
lengths: list how long that section of the staple should be
from-3end: if the first (:i i) start should be from the 5 end or 3 end
down: indices traveresd i-..."
  (let* ((helices (if down
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

(wmdna "./tmp/me" (list
                (5nt (first (scaffold tri)))
                (loop for i from 22 to 20 by 1
                      collect
                      (create-staple
                       (staple-ordered-antiparallel-strands
                        (scaffold tri)
                        i
                       '(24 17 24) '(8 15 8)
                        :down t)))
                ))

(create-staple
 (staple-ordered-antiparallel-strands
  (scaffold tri)
  22
  '(24 17 24) '(8 15 8)
  :down t))
