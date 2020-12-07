(in-package :small)

(let* ((start-coord (v3 0 0 0))
       (vaxis (v3 0 0 1))
       (vbb0 (v3 1 0 0)))
  (multiple-value-bind (strand nts)
      (SMALL::helix-strand start-coord vaxis vbb0 33)
    (mapcar #'vn nts)
    ))
