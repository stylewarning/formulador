;;;; formulador.asd
;;;;
;;;; Copyright (c) 2011-2014 Robert Smith

(defpackage #:formulador-asd
  (:use #:cl))

(in-package #:formulador-asd)

(asdf:defsystem #:formulador
  :name "formulador"
  :version "0.0.3"
  :maintainer "Robert Smith"
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (see LICENSE)"
  :description "Formula renderer."
  :long-description "A mathematical formula pretty printer."
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "region")
               (:file "canvas")
               (:file "charmap")
               (:file "boxes")
               (:file "blit")
               (:file "render")
               (:file "constructions")))
