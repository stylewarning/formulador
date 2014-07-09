;;;; canvas.lisp
;;;;
;;;; Copyright (c) 2013 Robert Smith
;;;;
;;;; A simple notion of a "canvas" on which we can draw formulas.

(in-package #:formulador)

(defstruct (canvas (:constructor %make-canvas)
                   (:predicate canvasp)
                   (:print-function (lambda (canvas stream depth)
                                      (declare (ignore depth))
                                      (print-canvas canvas stream))))
  data)

(defun make-canvas (width height)
  "Make a new canvas of width WIDTH and height HEIGHT."
  (%make-canvas :data (make-array (list height width)
                                  :element-type 'character
                                  :initial-element #\Space)))

(defun canvas-dimensions (canvas)
  "Return the dimensions of the canvas (WIDTH HEIGHT)."
  (reverse (array-dimensions (canvas-data canvas))))

(defun canvas-ref (canvas x y)
  "Obtain the character (X, Y) in the canvas CANVAS."
  (aref (canvas-data canvas) y x))

(defun canvas-set (canvas x y new-data)
  "Set the character at (X, Y) in the canvas CANVAS to the value NEW-DATA."
  (setf (aref (canvas-data canvas) y x)
        new-data))

(defsetf canvas-ref canvas-set)

(defun print-canvas (canvas &optional (stream *standard-output*))
  (destructuring-bind (width height)
      (canvas-dimensions canvas)
    (loop :initially (write-char #\+ stream) 
          :repeat width
          :do (write-char #\- stream)
          :finally (progn
                     (write-char #\+ stream)
                     (terpri stream)))
    
    (dotimes (y height)
      (write-char #\| stream)
      (dotimes (x width)
        (write-char (canvas-ref canvas x y) stream))
      (write-char #\| stream)
      (terpri stream))
    
    (loop :initially (write-char #\+ stream) 
          :repeat width
          :do (write-char #\- stream)
          :finally (progn
                     (write-char #\+ stream)
                     (terpri stream)))))
