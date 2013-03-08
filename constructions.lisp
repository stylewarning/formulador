;;;; constructions.lisp
;;;; Copyright (c) 2013 Robert Smith

(defun glue (&rest items)
  "Glue together (horizontally) all of ITEMS."
  (row-box items))

(defvar +sigma+
  (picture-box '("==="
                 "\\"
                 " >"
                 "/"
                 "==="))
  "The capital sigma symbol used for summation.")
