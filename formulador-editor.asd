;;;; formulador-editor.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:formulador-editor-asd
  (:use #:cl))

(in-package #:formulador-editor-asd)

(asdf:defsystem #:formulador-editor
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (see LICENSE)"
  :description "Formula editor."
  :long-description "A mathematical formula editor."
  :depends-on (#:formulador #:cl-charms)
  :serial t
  :components ((:module "editor"
                :serial t
                :components ((:file "package")
                             (:file "ncurses")
                             (:file "editor")))))
