;;;; editor/package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:formulador-editor
  (:documentation "An ncurses-based formula editor.")
  (:use #:cl #:formulador)

  ;; editor.lisp
  (:export
   #:start-editor))
