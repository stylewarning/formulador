;;;; formulador.asd
;;;;
;;;; Copyright (c) 2011-2018 Robert Smith

(asdf:defsystem #:formulador
  :version "0.0.3"
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (see LICENSE)"
  :description "A mathematical formula pretty printer."
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "region")
               (:file "canvas")
               (:file "charmap")
               (:file "boxes")
               (:file "blit")
               (:file "render")
               (:file "constructions")
               (:file "examples")))
