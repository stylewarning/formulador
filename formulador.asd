(defpackage #:formulador-asd
  (:use :cl :asdf))

(in-package :formulador-asd)

(defsystem formulador
  :name "formulador"
  :version "0.0.1"
  :maintainer "Robert Smith"
  :author "Robert Smith"
  :description "Formula renderer."
  :long-description "A mathematical formula pretty printer."
  :serial t
  :components ((:file "package")
               (:file "formulador")
               (:file "render"))
  :depends-on ("vecto"))
