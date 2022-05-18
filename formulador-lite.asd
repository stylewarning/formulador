;;;; formulador-lite.asd
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(asdf:defsystem #:formulador-lite
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "BSD 3-clause (see LICENSE)"
  :description "Simplified formula interface."
  :long-description "A simple input interface for Formulador."
  :depends-on (#:formulador #:alexa)
  :serial t
  :components ((:module "formulador-lite"
                :serial t
                :components ((:file "package")
			     (:file "lexical-analysis")
			     (:file "unit-detection")
			     (:file "blocks")
			     (:file "parentheses")
			     (:file "operators")
			     (:file "drawing")
			     (:file "tests")))))
