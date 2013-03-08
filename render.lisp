;;;; render.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

;;; Facility to render boxes.

(in-package #:formulador)

(defun draw (box)
  (let* ((width (width box))
         (height (height box))
         (canvas (make-canvas width height)))
    (blit canvas box 0 0)
    canvas))
