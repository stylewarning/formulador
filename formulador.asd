;;;; formulador.asd
;;;; Copyright (c) 2011-2013 Robert Smith

(defpackage #:formulador-asd
  (:use :cl :asdf))

(in-package :formulador-asd)

(defsystem formulador
  :name "formulador"
  :version "0.0.2"
  :maintainer "Robert Smith"
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (see LICENSE)"
  :description "Formula renderer."
  :long-description "A mathematical formula pretty printer."
  :serial t
  :components ((:file "package")
               (:file "canvas")
               (:file "boxes")
               (:file "blit")
               (:file "render")
               (:file "constructions")))
