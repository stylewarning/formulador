;;;; constructions.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun glue (a b)
  (row-box (list a b)))

(defvar +sigma+
  (picture-box '("==="
                 "\\"
                 " >"
                 "/"
                 "===")))
