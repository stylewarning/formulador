;;;; formulador-lite/package.lisp
;;;;
;;;; Copyright (c) 2021 Izaak Walton

(defpackage #:formulador-lite
  (:documentation "A simple-input interface for formula illustration")
  (:use #:cl #:formulador)

  ;; simple-draw.lisp
  (:export
   #:simple-draw)

  ;;interpreter.lisp
  (:export
   #:start-drawing))
