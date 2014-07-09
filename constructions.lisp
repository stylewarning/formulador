;;;; constructions.lisp
;;;;
;;;; Copyright (c) 2013-2014 Robert Smith
;;;;
;;;; Various constructions based off of primitive constructions.

(in-package #:formulador)

(defun glue (&rest items)
  "Glue together (horizontally) all of ITEMS."
  (row-box items))

(defvar +sigma+
  (picture-box '("==="
                 "\\"
                 " >"
                 "/"
                 "===")
               :baseline 2)
  "The capital sigma symbol used for summation.")
