;;;; render.lisp
;;;;
;;;; Copyright (c) 2011-2014 Robert Smith
;;;;
;;;; A facility to conveniently render boxes.

(in-package #:formulador)

(defun draw (box)
  (let* ((width (width box))
         (height (height box))
         (canvas (make-canvas width height)))
    (blit canvas box 0 0)
    canvas))
