;;;; constructions.lisp
;;;;
;;;; Copyright (c) 2013-2014 Robert Smith
;;;;
;;;; Various constructions based off of primitive constructions.

(in-package #:formulador)

(defun glue (&rest items)
  "Glue together (horizontally) all of ITEMS."
  (row-box items))

(defun tape (&rest items)
  "Tape together (horizontally) all of ITEMS."
  (row-box items :padding 1))

(defvar +center-dot+ (freeze (box (code-char #xB7))))

(defvar +cdots+ (freeze (glue +center-dot+
                              +center-dot+
                              +center-dot+)))

(defvar +partial+ (freeze (box (code-char #x2202))))

(defvar +sigma+
  (freeze
   (picture-box '("==="
                  "\\"
                  " >"
                  "/"
                  "===")
                :baseline 2))
  "The capital sigma symbol used for summation.")

(defvar +integral+
  (freeze
   (picture-box '("/"
                  "|"
                  "|"
                  "|"
                  "/")
                :baseline 2))
  "A standard integral symbol.")

(defvar +double-integral+
  (freeze
   (glue +integral+ +integral+))
  "A double integral symbol.")

(defvar +triple-integral+
  (freeze
   (glue +integral+ +integral+ +integral+))
  "A triple integral symbol.")

