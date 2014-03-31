(defpackage #:fisheye-hyperbolic
  (:nicknames hyperbolic)
  (:use #:cl))

;;;long-float is double-float in sbcl
(setf *read-default-float-format* 'long-float)

